package sculpt

object Compiler {
  type Program = Vector[Fun]

  def usedNames(pgm: Program): Map[String, Vector[String]] = {
    pgm.map { f =>
      f.name -> f.definedNames
    }.toMap
  }
}
