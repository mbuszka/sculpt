package sculpt

import chisel3._
import fastparse._
import java.io._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

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
    chisel3.Driver.execute(chiselArgs, () => Compiler.compile(pgm))
  }
}