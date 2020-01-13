package sculpt

sealed trait Def
case class Fun(name: String, args: Vector[String], body: Expr) extends Def

sealed trait Expr
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

sealed trait Pattern 
case class PatWild(expr: Expr) extends Pattern
case class PatConstant(const: Const, expr: Expr) extends Pattern
case class PatConstructor(tag: String, vars: Vector[String], expr: Expr)
    extends Pattern
