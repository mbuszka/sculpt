package sculpt

import cats._
import cats.implicits._

case class Fun(name: String, args: Vector[String], body: Expr) {
  lazy val definedNames: Vector[String] =
    (args.toSet ++ body.definedNames).toVector
  lazy val regMap: Map[String, Int] = definedNames.zipWithIndex.toMap
  lazy val parameters: Vector[Int] = args.map(regMap)
  def usedTags: Set[String] = body.usedTags
  def maxStructSize: Int = body.maxStructSize
}

sealed trait Expr {
  def definedNames: Set[String] = this match {
    case Call(fun, args)        => Set()
    case Match(value, branches) => branches.foldMap(_.definedNames)
    case Let(name, prim, body)  => body.definedNames + name
    case Halt(value)            => Set()
  }

  def usedTags: Set[String] = this match {
    case Call(fun, args)        => Set()
    case Match(value, branches) => branches.foldMap(_.usedTags)
    case Let(name, prim, body)  => prim.usedTags
    case Halt(value)            => Set()
  }

  def maxStructSize: Int = this match {
    case Call(fun, args)        => 0
    case Match(value, branches) => branches.map(_.maxStructSize).max
    case Let(name, prim, body)  => prim.maxStructSize.max(body.maxStructSize)
    case Halt(value)            => 0
  }
}

case class Call(fun: String, args: Vector[Atom]) extends Expr
case class Match(value: Atom, branches: Vector[Pattern]) extends Expr
case class Let(name: String, prim: Prim, body: Expr) extends Expr
case class Halt(value: Atom) extends Expr

sealed trait Prim {
  def usedTags: Set[String] = this match {
    case PrimOp(op, args)       => Set()
    case Constructor(tag, args) => Set(tag)
  }

  def maxStructSize: Int = this match {
    case PrimOp(op, args)       => 0
    case Constructor(tag, args) => args.size + 1
  }
}

case class PrimOp(op: Op, args: Vector[Atom]) extends Prim
case class Constructor(tag: String, args: Vector[Atom]) extends Prim

sealed trait Op
case object Add extends Op
case object Sub extends Op
case object Nop extends Op

sealed trait Atom
case class Var(name: String) extends Atom
case class Const(value: Int) extends Atom

sealed trait Pattern {
  def definedNames: Set[String] = this match {
    case PatWild(expr)                   => expr.definedNames
    case PatConstant(const, expr)        => expr.definedNames
    case PatConstructor(tag, vars, expr) => vars.toSet ++ expr.definedNames
  }

  def usedTags: Set[String] = this match {
    case PatWild(expr)                   => expr.usedTags
    case PatConstant(const, expr)        => expr.usedTags
    case PatConstructor(tag, vars, expr) => expr.usedTags + tag
  }

  def maxStructSize: Int = this match {
    case PatWild(expr)            => expr.maxStructSize
    case PatConstant(const, expr) => expr.maxStructSize
    case PatConstructor(tag, vars, expr) =>
      expr.maxStructSize.max(vars.size + 1)
  }
}

case class PatWild(expr: Expr) extends Pattern
case class PatConstant(const: Const, expr: Expr) extends Pattern
case class PatConstructor(tag: String, vars: Vector[String], expr: Expr)
    extends Pattern
