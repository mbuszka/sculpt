package sculpt

import chisel3._
import collection.mutable
import sculpt.Action.Update
import sculpt.Action.Move
import sculpt.Action.Fail
import sculpt.Source.Register
import sculpt.Source.Immediate
import chisel3.internal.firrtl.Width
import chisel3.util.log2Up
import chisel3.util.MuxLookup

sealed trait Action
object Action {
  case class Update(dst: Int, op: Op, lhs: Source, rhs: Source) extends Action
  case class Move(dst: Int, src: Source) extends Action
  case class Call(proc: String, args: Vector[Source]) extends Action
  case class Match(
      cond: Source,
      branches: Vector[(Int, Int)],
      default: Int
  ) extends Action
  case class Halt(res: Source) extends Action
  case object Fail extends Action
}

sealed trait Source
object Source {
  case class Register(num: Int) extends Source
  case class Immediate(value: Int) extends Source
}

case class Block(actions: Vector[Action], parameters: Vector[Int])

object Compiler {
  type Program = Vector[Fun]

  def usedNames(pgm: Program): Map[String, Vector[String]] = {
    pgm.map { f =>
      f.name -> f.definedNames
    }.toMap
  }

  def actions(f: Fun): (String, Block) = {
    f.name -> Block(ExprCompiler(f.regMap)(f.body), f.parameters)
  }

  def compile(pgm: Program): Result = {
    val intWidth = 32.W
    val regCnt = usedNames(pgm).values.map(_.length).max
    val blocks = pgm.map(actions)
    // pprint.pprintln(blocks)
    new Result(blocks, regCnt)
  }

  def source(regs: Vector[SInt])(s: Source): SInt = s match {
    case Register(num)    => regs(num)
    case Immediate(value) => value.asSInt()
  }
}

case class ResIO(intWidth: Int, stateWidth: Int, regCnt: Int) extends Bundle {
  val address = Input(UInt(8.W))
  // val read = Input(Bool())
  val readData = Output(SInt(intWidth.W))
  val write = Input(Bool())
  val writeData = Input(SInt(intWidth.W))
}

class Result(blocks: Vector[(String, Block)], regCnt: Int) extends Module {
  val intWidth = 32

  val bMap = blocks.toMap
  val indices = blocks.flatMap { case (n, b) => b.actions.indices.map(n -> _) }
  val actions = blocks.flatMap {
    case (n, b) =>
      b.actions.zipWithIndex.map {
        case (a, idx) => (n, idx) -> a
      }
  }.toMap
  val sIdle :: sFail :: rest = Chisel.Enum(indices.size + 2)
  val stIdle :: stBusy :: stFailed :: Nil = Chisel.Enum(3)
  val getState = indices.zip(rest).toMap
  val registers = Vector.fill(regCnt)(RegInit(0.asSInt(intWidth.W)))
  val state = RegInit(sIdle)
  val io = IO(new ResIO(intWidth, state.getWidth, regCnt))
  val result = Reg(SInt(intWidth.W))
  val next = state + 1.asUInt()
  val status = RegInit(stIdle)

  io.readData := MuxLookup(
    io.address,
    status.asSInt(),
    registers.zipWithIndex.map {
      case (r, idx) => idx.U -> r
    } ++ Map("hFE".U -> result, "hFF".U -> status.asSInt())
  )

  val init = when(state === sIdle) {
    when(io.write) {
      var c = when(io.address === "hFF".U && (status =/= stBusy)) {
        state := getState(("main", 0))
        status := stBusy
      }
      for (i <- 0.until(regCnt)) {
        c = c.elsewhen(io.address === i.U) {
          registers(i) := io.writeData
        }
      }
    }
  }
  indices.foldLeft(init) { (before, idx) =>
    actions(idx) match {
      case Update(dst, Add, lhs, rhs) =>
        before.elsewhen(state === getState(idx)) {
          registers(dst) := Compiler.source(registers)(lhs) + Compiler.source(
            registers
          )(rhs)
          state := next
        }
      case Update(dst, Sub, lhs, rhs) =>
        before.elsewhen(state === getState(idx)) {
          registers(dst) := Compiler.source(registers)(lhs) - Compiler.source(
            registers
          )(rhs)
          state := next
        }
      case Move(dst, src) => ???
      case sculpt.Action.Call(proc, args) =>
        val targets =
          bMap(proc).parameters.zip(args.map(Compiler.source(registers)))
        before.elsewhen(state === getState(idx)) {
          targets.foreach {
            case (dst, value) => registers(dst) := value
          }
          state := getState((proc, 0))
        }
      case sculpt.Action.Match(cond, branches, default) =>
        val c = Compiler.source(registers)(cond)
        before.elsewhen(state === getState(idx)) {
          if (branches.isEmpty) state := state + default.asUInt()
          else {
            val (fst, off) = branches.head
            val init = when(fst.asSInt() === c) {
              state := state + off.asUInt()
            }
            branches.tail
              .foldLeft(init) { (before, p) =>
                val (cnd, off) = p
                before.elsewhen(cnd.asSInt() === c) {
                  state := state + off.asUInt()
                }
              }
              .otherwise {
                state := state + default.asUInt()
              }
          }
        }
      case sculpt.Action.Halt(res) =>
        before.elsewhen(state === getState(idx)) {
          state := sIdle
          status := stIdle
          result := Compiler.source(registers)(res)
        }
      case Fail =>
        before.elsewhen(state === getState(idx)) {
          state := sIdle
          status := stFailed
        }
    }
  }
}

case class ExprCompiler(registers: Map[String, Int]) {
  def apply(expr: Expr): Vector[Action] = expr match {
    case Call(fun, args) =>
      Vector(Action.Call(fun, args.map(translate)))
    case Match(value, branches) =>
      compileBranches(translate(value), branches)
    case Let(name, PrimOp(op, Vector(lhs, rhs)), body) =>
      Action
        .Update(registers(name), op, translate(lhs), translate(rhs)) +: apply(
        body
      )
    case Halt(value) =>
      Vector(Action.Halt(translate(value)))
  }

  def translate(atom: Atom): Source = atom match {
    case Const(value) => Source.Immediate(value)
    case Var(name)    => Source.Register(registers(name))
  }

  def compileBranches(
      value: Source,
      branches: Vector[Pattern]
  ): Vector[Action] = {
    val buffer = mutable.Buffer[Action]()
    val offsets = mutable.Buffer[(Int, Int)]()

    def go(branches: Vector[Pattern], offset: Int): Int = {
      if (branches.isEmpty) {
        buffer.append(Action.Fail)
        offset
      } else {
        branches.head match {
          case PatWild(expr) =>
            buffer.appendAll(apply(expr))
            offset
          case PatConstant(Const(const), expr) =>
            offsets.append((const, offset))
            val as = apply(expr)
            buffer.appendAll(as)
            go(branches.tail, offset + as.size)
          case PatConstructor(tag, vars, expr) =>
            throw new Exception("Not implemented yet")
        }
      }
    }
    val default = go(branches, 1)
    Action.Match(value, offsets.toVector, default) +: buffer.toVector
  }
}
