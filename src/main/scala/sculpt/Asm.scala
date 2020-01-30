package sculpt

import collection.mutable
import sculpt.Src._
import scala.annotation.tailrec

sealed trait Asm

object Asm {
  case class Update(target: Option[Label], assignment: Vector[Assign])
      extends Asm

  case class Switch(
      scrutinee: Src,
      branches: Vector[(Int, Label)],
      default: Label
  ) extends Asm

  case class Load(address: Src) extends Asm
  case class Store(addressRegister: Int, tag: Int, values: Vector[Src])
      extends Asm
  case class Halt(result: Src) extends Asm
  case object Fail extends Asm
}

sealed trait Src
object Src {
  case class Reg(index: Int) extends Src
  case class Imm(value: Int) extends Src
  case class Mem(offset: Int) extends Src
}

case class Label(procedure: String, block: Int)
case class Assign(destination: Int, op: Op, sources: Vector[Src])
case class Block(label: Label, asm: Vector[Asm])
case class Assembly(
    offsets: Map[Label, Int],
    instructions: Vector[Asm],
    size: Int
)

object Compile {
  def apply(program: Program): Vector[Block] = {
    val parameters = Analysis.procedureParameters(program)
    val tags = Analysis.usedTags(program)
    program.flatMap { f =>
      val compile = new FunCompilerClosure(f.name, f.regMap, parameters, tags)
      compile(f.body)
    }
  }
}

object Assemble {
  def apply(blocks: Vector[Block]): Assembly = {
    go(blocks.toList, Vector(), Map(), 0)
  }

  @tailrec private def go(
      blocks: List[Block],
      asm: Vector[Asm],
      offsets: Map[Label, Int],
      offset: Int
  ): Assembly = blocks match {
    case Nil =>
      assert(offset == asm.size)
      Assembly(offsets, asm, offset)
    case Block(label, body) :: rest =>
      go(rest, asm ++ body, offsets + (label -> offset), offset + body.size)
  }
}

class FunCompilerClosure(
    procedure: String,
    registerMap: Map[String, Int],
    calleeRegisters: Map[String, Vector[Int]],
    programTags: Map[String, Int]
) {
  private var nextLabelIndex = 1
  private val blocks = mutable.Buffer[Block]()

  def apply(expr: Expr): Vector[Block] = {
    val body = expression(expr, Vector())
    blocks.append(Block(Label(procedure, 0), body))
    blocks.toVector
  }

  private def nextLabel(): Label = {
    val idx = nextLabelIndex
    nextLabelIndex += 1
    Label(procedure, idx)
  }

  private def reg(name: String): Int = {
    registerMap(name)
  }

  private def atom(a: Atom): Src = a match {
    case Var(name)    => Reg(reg(name))
    case Const(value) => Imm(value)
  }

  private def let(name: String, p: Prim): Asm = p match {
    case PrimOp(op, args) =>
      Asm.Update(None, Vector(Assign(reg(name), op, args.map(atom))))
    case Constructor(tag, args) =>
      Asm.Store(reg(name), programTags(tag), args.map(atom))
  }

  private def expression(expr: Expr, buffer: Vector[Asm]): Vector[Asm] = {
    expr match {
      case Call(fun, args) =>
        val assignments = args.zip(calleeRegisters(fun)).map {
          case (a, reg) => Assign(reg, Nop, Vector(atom(a)))
        }
        buffer :+ Asm.Update(Some(Label(fun, 0)), assignments)
      case Match(value, branches) =>
        switch(value, branches, buffer)
      case Let(name, prim, body) =>
        expression(body, buffer :+ let(name, prim))
      case Halt(value) =>
        buffer :+ Asm.Halt(atom(value))
    }
  }

  private def switch(
      scrutinee: Atom,
      branches: Vector[Pattern],
      instructions: Vector[Asm]
  ): Vector[Asm] = {
    val jumps = mutable.Buffer[(Int, Label)]()

    def go(bs: Vector[Pattern], tp: MatchType): (MatchType, Label) = {
      val lbl = nextLabel()
      if (bs.isEmpty) {
        blocks.append(Block(lbl, Vector(Asm.Fail)))
        (tp, lbl)
      } else {
        bs.head match {
          case PatWild(expr) =>
            blocks.append(Block(lbl, expression(expr, Vector())))
            (tp, lbl)
          case PatConstant(Const(const), expr) if tp != MatchTag =>
            blocks.append(Block(lbl, expression(expr, Vector())))
            jumps.append((const, lbl))
            go(bs.tail, MatchInt)
          case PatConstructor(tag, vars, expr) if tp != MatchInt =>
            val assignments = vars.zipWithIndex.map {
              case (name, offset) =>
                Assign(reg(name), Nop, Vector(Mem(offset)))
            }
            jumps.append((programTags(tag), lbl))
            val asm = expression(expr, Vector(Asm.Update(None, assignments)))
            blocks.append(Block(lbl, asm))
            go(bs.tail, MatchTag)
          case _ =>
            throw new Exception("Incompatible branches of pattern match")
        }
      }
    }
    val (tp, default) = go(branches, MatchIgnore)
    tp match {
      case MatchTag =>
        instructions ++ Vector(
          Asm.Load(atom(scrutinee)),
          Asm.Switch(Mem(0), jumps.toVector, default)
        )
      case _ =>
        instructions :+ Asm.Switch(atom(scrutinee), jumps.toVector, default)
    }
  }

  private trait MatchType
  private case object MatchIgnore extends MatchType
  private case object MatchInt extends MatchType
  private case object MatchTag extends MatchType
}
