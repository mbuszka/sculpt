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

  def num[_: P]: P[Int] = P((P("-").? ~ CharIn("0-9").rep(1)).!.map(_.toInt))

  def tag[_: P]: P[String] =
    P(CharPred(_.isUpper).! ~ CharIn("0-9a-zA-Z_\\-").rep.!).map {
      case (l, r) => l ++ r
    }

  def ident[_: P]: P[String] =
    P(CharPred(_.isLower).! ~ CharIn("0-9a-zA-Z_\\-").rep.!).map {
      case (l, r) => l ++ r
    }

  def op[_: P]: P[Op] = P(P("+").map(_ => Add) | P("-").map(_ => Sub))

  def atom[_: P]: P[Atom] = P(ident.map(Var) | num.map(Const))

  def prim[_: P]: P[Prim] = P(
    parens(P(op ~ atom.rep(1).map(_.toVector)).map(PrimOp.apply _ tupled)) |
      parens(P(tag ~ atom.rep.map(_.toVector)).map(Constructor.apply _ tupled))
  )

  def expr[_: P]: P[Expr] = P(
    parens(P("let" ~ ident ~ prim ~ expr).map(Let.apply _ tupled)) |
      parens(
        P("match" ~ atom ~ branch.rep(1).map(_.toVector))
          .map(Match.apply _ tupled)
      )
  )

  def branch[_: P]: P[Pattern] = P(
    parens(
      P(tag ~ ident.rep.map(_.toVector) ~ expr)
        .map(PatConstructor.apply _ tupled)
    ) |
    parens(P(num.map(Const) ~ expr).map(PatConstant.apply _ tupled)) |
    parens(P("else" ~ expr).map(PatWild(_)))
  )

  def parens[A, _: P](inner: => P[A]): P[A] = P("(" ~ inner ~ ")")
}
