package sculpt

import cats.implicits._

object Analysis {
  def procedureParameters(program: Program): Map[String, Vector[Int]] = {
    program.map(f => f.name -> f.parameters).toMap
  }

  def usedTags(program: Program): Map[String, Int] = {
    program.foldMap(_.usedTags).zipWithIndex.toMap
  }

  def requiredRegisters(program: Program): Int = {
    program.map(_.definedNames.size).max
  }

  def maxStructSize(program: Program): Int = {
    program.map(_.maxStructSize).max
  }
}