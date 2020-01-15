package sculpt

import cats._
import cats.implicits._

sealed trait Def
case class Fun(name: String, args: Vector[String], body: Expr) extends Def {
  def definedNames: Vector[String] = (args.toSet ++ body.definedNames).toVector
  lazy val regMap: Map[String, Int] = definedNames.zipWithIndex.toMap
  lazy val parameters: Vector[Int] = args.map(regMap)
}

sealed trait Expr {
  def definedNames: Set[String] = this match {
    case Call(fun, args)        => Set()
    case Match(value, branches) => branches.foldMap(_.definedNames)
    case Let(name, prim, body)  => body.definedNames + name
    case Halt(value)            => Set()
  }
}
case class Call(fun: String, args: Vector[Atom]) extends Expr
case class Match(value: Atom, branches: Vector[Pattern]) extends Expr
case class Let(name: String, prim: Prim, body: Expr) extends Expr
case class Halt(value: Atom) extends Expr

sealed trait Prim
case class PrimOp(op: Op, args: Vector[Atom]) extends Prim
case class Constructor(tag: String, args: Vector[Atom]) extends Prim

sealed trait Op
case object Add extends Op
case object Sub extends Op

sealed trait Atom
case class Var(name: String) extends Atom
case class Const(value: Int) extends Atom

sealed trait Pattern {
  def definedNames: Set[String] = this match {
    case PatWild(expr)                   => Set()
    case PatConstant(const, expr)        => expr.definedNames
    case PatConstructor(tag, vars, expr) => vars.toSet ++ expr.definedNames
  }
}
case class PatWild(expr: Expr) extends Pattern
case class PatConstant(const: Const, expr: Expr) extends Pattern
case class PatConstructor(tag: String, vars: Vector[String], expr: Expr)
    extends Pattern
