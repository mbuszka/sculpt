package sculpt

sealed trait Scrutinee

case class Store(impl: Vector[Struct]) {
  def alloc(s: Struct): (Store, Addr) = (Store(impl :+ s), Addr(impl.size))
  def apply(addr: Addr): Struct = impl(addr.ref)
}

object Store {
  def apply(): Store = Store(Vector())
}

sealed trait Value {
  def asInt: Int = this match {
    case Num(value) => value
    case _ => throw new Exception("Expected value to be a number")
  }
}
case class Addr(ref: Int) extends Value
case class Num(value: Int) extends Value with Scrutinee

case class Struct(tag: String, fields: Vector[Value]) extends Scrutinee

object Eval {
  def apply(
      code: Map[String, Fun],
      env: Map[String, Value],
      store: Store,
      expr: Expr
  ): Value = {
    // println("Evaluating:")
    // pprint.pprintln(expr)
    // pprint.pprintln(env)
    expr match {
      case Halt(a) => atom(env)(a)
      case Call(fun, args) => 
        val d = code(fun)
        Eval(code, d.args.zip(args.map(atom(env))).toMap, store, d.body)
      case Match(a, branches) =>
        branch(code, env, store, scrutinee(env, store)(a), branches)
      case Let(name, prim, body) =>
        val (v, s) = prim match {
          case PrimOp(Add, Vector(l, r)) => (Num(atom(env)(l).asInt + atom(env)(r).asInt), store)
          case PrimOp(Sub, Vector(l, r)) => (Num(atom(env)(l).asInt - atom(env)(r).asInt), store)
          case Constructor(tag, args) =>
            val struct = Struct(tag, args.map(atom(env)))
            val (newStore, addr) = store.alloc(struct)
            (addr, newStore)
        }
        Eval(code, env + (name -> v), s, body)
    }
  }

  def branch(
      code: Map[String, Fun],
      env: Map[String, Value],
      store: Store,
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
        case PatWild(body) :: tail => Eval(code, env, store, body)
      }
    }
    go(branches.toList)
  }

  def scrutinee(env: Map[String, Value], store: Store)(a: Atom): Scrutinee = a match {
    case Const(value) => Num(value)
    case Var(name) => env(name) match {
      case n@Num(_) => n
      case a@Addr(_) => store(a)
    }
  }

  def atom(env: Map[String, Value])(a: Atom): Value = a match {
    case Var(name) => env(name)
    case Const(value) => Num(value)
  } 
}