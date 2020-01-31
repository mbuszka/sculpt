package sculpt

import fastparse._

/** DEF  ::= ( 'def' IDENT ( IDENT + ) EXPR )
  *  EXPR ::= ( IDENT ATOM + )
  *         | ( 'match' ATOM BRANCH + )
  *         | ( 'let' IDENT PRIM EXPR )
  *  PRIM ::= ( OP ATOM + )
  *         | ( TAG ATOM * )
  *  ATOM ::= IDENT | NUM
  *  OP   ::= '+' | '-'
  *  BRANCH ::= ( TAG IDENT * EXPR )
  *           | ( NUM EXPR )
  *           | ( 'else' EXPR )
  *  TAG   ::= uppercase ident
  *  IDENT ::= lowercase ident
  *
  */
object Parser {
  import MultiLineWhitespace._

  def num[_: P]: P[Int] = P((P("-").? ~~ CharIn("0-9").repX(1)).!.map(_.toInt))

  def tag[_: P]: P[String] =
    P(CharPred(_.isUpper).! ~~ CharIn("0-9a-zA-Z_\\-").repX.!).map {
      case (l, r) => l ++ r
    }

  def ident[_: P]: P[String] =
    P(CharPred(_.isLower).! ~~ CharIn("0-9a-zA-Z_\\-").repX.!).map {
      case (l, r) => l ++ r
    }

  def op[_: P]: P[Op] = P(P("+").map(_ => Add) | P("-").map(_ => Sub))

  def atom[_: P]: P[Atom] = P(ident.map(Var) | num.map(Const))

  def prim[_: P]: P[Prim] = P(
    parens(P(op ~ atom.rep(1).map(_.toVector)).map(PrimOp.apply _ tupled) |
      P(tag ~ atom.rep.map(_.toVector)).map(Constructor.apply _ tupled))
  )

  def expr[_: P]: P[Expr] = P(
    parens(
      P("let" ~/ ident ~ prim ~ expr).map(Let.apply _ tupled) |
        P("match" ~/ atom ~ branch.rep(1).map(_.toVector))
          .map(Match.apply _ tupled) |
        P("halt" ~/ atom).map(Halt(_)) |
        P(ident ~/ atom.rep.map(_.toVector)).map(Call.apply _ tupled)
    )
  )

  def branch[_: P]: P[Pattern] = P(
    parens(
      P(tag ~/ ident.rep.map(_.toVector) ~ expr)
        .map(PatConstructor.apply _ tupled) |
        P(num.map(Const) ~/ expr).map(PatConstant.apply _ tupled) |
        P("else" ~/ expr).map(PatWild(_))
    )
  )

  def defn[_: P]: P[Fun] =
    parens(
      P("def" ~/ ident ~/ parens(P(ident.rep).map(_.toVector)) ~/ expr)
        .map(Fun.apply _ tupled)
    )

  def program[_: P]: P[Vector[Fun]] =
    P(Start ~ defn.rep(1).map(_.toVector) ~ End)

  def parens[A, _: P](inner: => P[A]): P[A] = P("(" ~/ inner ~ ")")
}
