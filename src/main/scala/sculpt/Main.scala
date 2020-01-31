package sculpt

import chisel3._
import fastparse._
import java.io._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

object Pipeline {
  def apply(program: Program, verbose: Boolean = false): Result = {
    val asm = Assemble(Compile(program))
    if (verbose) pprint.pprintln(asm, height = 500)
    val regCount = Analysis.requiredRegisters(program)
    val structSize = Analysis.maxStructSize(program).max(1)
    new Result(asm, regCount, structSize)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val fileName = args.head
    val chiselArgs = args.tail
    val pgm = parse(new FileInputStream(fileName), Parser.program(_)) match {
      case f: Failure =>
        println("Could not parse file")
        pprint.pprintln(f, height = 1000)
        return ()
      case Success(value, index) =>
        value
    }
    chisel3.Driver.execute(chiselArgs, () => Pipeline(pgm))
  }
}