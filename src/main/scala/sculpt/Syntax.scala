package sculpt

import cats._
import cats.implicits._


case class Fun(name: String, args: Vector[String], body: Expr) {
  lazy val definedNames: Vector[String] = (args.toSet ++ body.definedNames).toVector
  lazy val regMap: Map[String, Int] = definedNames.zipWithIndex.toMap
  lazy val parameters: Vector[Int] = args.map(regMap)
  def usedTags: Set[String] = body.usedTags
}


sealed trait Expr {
  def definedNames: Set[String] = this match {
    case Call(fun, args)        => Set()
    case Match(value, branches) => branches.foldMap(_.definedNames)
    case Let(name, prim, body)  => body.definedNames + name
    case Halt(value)            => Set()
  }

  def usedTags: Set[String] = this match {
    case Call(fun, args) => Set()
    case Match(value, branches) => branches.flatMap(_.usedTags).toSet
    case Let(name, prim, body) => prim.usedTags
    case Halt(value) => Set()
  }
}

case class Call(fun: String, args: Vector[Atom]) extends Expr
case class Match(value: Atom, branches: Vector[Pattern]) extends Expr
case class Let(name: String, prim: Prim, body: Expr) extends Expr
case class Halt(value: Atom) extends Expr


sealed trait Prim {
  def usedTags: Set[String] = this match {
    case PrimOp(op, args) => Set()
    case Constructor(tag, args) => Set(tag)
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
    case PatWild(expr)                   => Set()
    case PatConstant(const, expr)        => expr.definedNames
    case PatConstructor(tag, vars, expr) => vars.toSet ++ expr.definedNames
  }

  def usedTags: Set[String] = this match {
    case PatConstructor(tag, vars, expr) => Set(tag)
    case _ => Set()
  }
}

case class PatWild(expr: Expr) extends Pattern
case class PatConstant(const: Const, expr: Expr) extends Pattern
case class PatConstructor(tag: String, vars: Vector[String], expr: Expr)
    extends Pattern
