sealed trait Def
case class Fun(name: String, args: Vector[String], body: Expr) extends Def

sealed trait Expr
case class Call(fun: String, args: Vector[Atom]) extends Expr
case class Match(value: Atom, branches: Vector[Pattern]) extends Expr
case class Let(name: String, prim: Prim, body: Expr) extends Expr

sealed trait Prim
case class PrimOp(op: Op, args: Vector[Atom])
case class Constructor(tag: String, args: Vector[Atom])

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

sealed trait Scrutinee

sealed trait Value
case class Addr(ref: Int) extends Value
case class Num(value: Int) extends Value with Scrutinee

sealed trait Storable
case class Struct(tag: String, fields: Vector[Value]) extends Storable with Scrutinee

object Eval {
  def apply(
      code: Map[String, Fun],
      env: Map[String, Value],
      store: Map[Addr, Storable],
      expr: Expr
  ): Value = {
    expr match {
      case Call(fun, args) => 
        val d = code(fun)
        Eval(code, d.args.zip(args.map(atom(env))).toMap, store, d.body)
      case Match(a, branches) =>
        val v = atom(env)(a) 
        branch(code, env, store, ???, branches)  
    }
  }

  def branch(
      code: Map[String, Fun],
      env: Map[String, Value],
      store: Map[Addr, Storable],
      s: Scrutinee,
      branches: Vector[Pattern]
    ): Value = {
    def go(branches: List[Pattern]): Value = {
      branches match {
        case Nil => throw new Exception("Non-exhaustive pattern match!")
        case PatConstant(Const(n), body) :: tail => s match {
          case Num(value) if (value == n) => Eval(code, env, store, body)
          case _ => go(tail)
        }
        case PatConstructor(t, vs, body) :: tail => s match {
          case Struct(t1, vals) if t == t1 => Eval(code, env ++ vs.zip(vals), store, body)
          case _ => go(tail)
        }
      }
    }
    go(branches.toList)
  }

  def atom(env: Map[String, Value])(a: Atom): Value = a match {
    case Var(name) => env(name)
    case Const(value) => Num(value)
  } 
}
