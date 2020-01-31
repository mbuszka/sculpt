package sculpt

import chisel3._
import collection.mutable
import chisel3.internal.firrtl.Width
import chisel3.util.log2Up
import chisel3.util.MuxLookup

case class MemoryIO(intWidth: Int, structSize: Int, addrSize: Int)
    extends Bundle {
  val address = Input(UInt(addrSize.W))
  val readData = Output(Vec(structSize, SInt(intWidth.W)))
  val write = Input(Bool())
  val writeData = Input(Vec(structSize, SInt(intWidth.W)))
}

class Memory(intWidth: Int, structSize: Int, addrSize: Int) extends Module {
  val io = IO(MemoryIO(intWidth, structSize, addrSize))
  val readAddr = RegNext(io.address)
  val mem = SyncReadMem(1 << addrSize, Vec(structSize, SInt(io.intWidth.W)))
  io.readData := mem.read(readAddr)
  when(io.write) {
    mem.write(io.address, io.writeData)
  }
}

case class ResIO(intWidth: Int) extends Bundle {
  val address = Input(UInt(8.W))
  // val read = Input(Bool())
  val readData = Output(SInt(intWidth.W))
  val write = Input(Bool())
  val writeData = Input(SInt(intWidth.W))
}

abstract class Status(int: Int) {
  val width = 2.W
  def S: SInt = int.S(width)
  def U: UInt = int.U(width)
}
object Status {
  case object Idle extends Status(0)
  case object Busy extends Status(1)
  case object Failed extends Status(2)
}

sealed trait State {
  def rep: UInt
}
object State {
  case class Idle(rep: UInt) extends State
  case class Instruction(asm: Asm, rep: UInt) extends State
}

class States(asm: Assembly) {
  val idle: State = State.Idle(asm.size.U)
  val all: Vector[State] =
    idle +: asm.instructions.zipWithIndex.map {
      case (a, idx) => State.Instruction(a, idx.U)
    }
  def jump(label: Label): UInt = asm.offsets(label).U
}

class Result(asm: Assembly, regCnt: Int, structSize: Int) extends Module {
  val intWidth = 32
  val addrSize = 10

  // State machine states
  val states = new States(asm)
  val state = RegInit(states.idle.rep)
  val next = state + 1.U

  // Communication with outside world
  val io = IO(new ResIO(intWidth))
  val result = Reg(SInt(intWidth.W))
  val status = RegInit(Status.Idle.U)

  // Register file
  val registers = Vector.fill(regCnt)(RegInit(0.S(intWidth.W)))

  // Memory WIP
  val mem = Module(new Memory(intWidth, structSize, addrSize))
  val nextFree = RegInit(UInt(addrSize.W), 0.U)
  val readAddr = RegInit(UInt(addrSize.W), 0.U)
  mem.io.address := readAddr
  mem.io.write := false.B
  mem.io.writeData := VecInit(Seq.fill(structSize)(0.S))
  val stall = RegInit(0.U(2.W))

  // Output routing
  io.readData := MuxLookup(
    io.address,
    status.asSInt(),
    registers.zipWithIndex.map {
      case (r, idx) => idx.U -> r
    } ++ Map(
      "hFE".U -> result,
      "hFF".U -> status.asSInt()
    )
  )

  // State machine transition
  conditional(states.all) { s =>
    state === s.rep
  } {
    case State.Idle(_) => idleState()
    case State.Instruction(asm, _) =>
      asm match {
        case Asm.Update(jumpTo, assignments) =>
          assignments.foreach {
            case Assign(reg, Add, Vector(l, r)) =>
              registers(reg) := src(l) + src(r)
            case Assign(reg, Sub, Vector(l, r)) =>
              registers(reg) := src(l) - src(r)
            case Assign(reg, Nop, Vector(s)) =>
              registers(reg) := src(s)
          }
          jumpTo match {
            case None        => state := next
            case Some(label) => state := states.jump(label)
          }
        case Asm.Switch(s, branches, default) =>
          val check = src(s)
          conditional(branches)(_._1.S === check) {
            case (_, lbl) => state := states.jump(lbl)
          }.otherwise {
            state := states.jump(default)
          }
        case Asm.Load(addr) =>
          stall := stall + 1.U
          when(stall === 0.U) {
            readAddr := src(addr).asUInt()
          }.elsewhen(stall === 3.U) {
            state := next
          }
        case Asm.Store(addressRegister, tag, values) =>
          registers(addressRegister) := nextFree.asSInt()
          mem.io.writeData(0) := tag.S
          values.zipWithIndex.foreach {
            case (s, i) => mem.io.writeData(i + 1) := src(s)
          }
          mem.io.address := nextFree
          mem.io.write := true.B
          nextFree := nextFree + 1.U
          state := next
        case Asm.Halt(res) =>
          result := src(res)
          state := states.idle.rep
          status := Status.Idle.U
        case Asm.Fail =>
          state := states.idle.rep
          status := Status.Failed.U
      }
  }

  private def src(s: Src): SInt = s match {
    case Src.Reg(index)  => registers(index)
    case Src.Imm(value)  => value.S
    case Src.Mem(offset) =>
      mem.io.readData(offset)
  }

  // handle register initialization and execution start
  private def idleState(): Unit = {
    when(io.write) {
      var c = when(io.address === "hFF".U) {
        state := states.jump(Label("main", 0))
        status := Status.Busy.U
      }
      for (i <- 0.until(regCnt)) {
        c = c.elsewhen(io.address === i.U) {
          registers(i) := io.writeData
        }
      }
    }
  }

  private def conditional[A](cases: Vector[A])(cond: A => Bool)(
      body: A => Unit
  ): WhenContext = {
    if (cases.isEmpty) throw new Exception("Cannot build conditional")
    else {
      val x = cases.head
      cases.tail.foldLeft(when(cond(x))(body(x))) { (before, x) =>
        before.elsewhen(cond(x))(body(x))
      }
    }
  }
}
