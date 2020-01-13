package sculpt

import org.scalatest.{WordSpec, Matchers}
import fastparse._

class Spec extends WordSpec with Matchers {
  "Parser" should {
    "parse numbers" in {
      parse("123", Parser.num(_)) shouldBe a[fastparse.Parsed.Success[_]]
      parse("-5", Parser.num(_)) shouldBe a[fastparse.Parsed.Success[_]]
      parse("foo", Parser.num(_)) shouldBe a[fastparse.Parsed.Failure]
    }

    "parse expr" in {
      parse("(foo 5 6 )", Parser.expr(_)).get.value shouldBe Call(
        "foo",
        Vector(Const(5), Const(6))
      )
    }

    "parse program" in {
      val pgm =
        """
(def go (n m acc)
  (match m
    (0 (halt acc))
    (else 
      (let m (- m 1)
        (let acc (+ n acc)
          (go n m acc))))))
(def mul (n m)
  (go n m 0))
        """
      val res = parse(pgm, Parser.program(_)).get.value
      val defs = res.map(f => f.name -> f).toMap
      pprint.pprintln(defs)
      val exp = parse("(mul 3 2)", Parser.expr(_)).get.value
      val v = Eval(defs, Map(), Store(), exp)
      pprint.pprintln(v)
    }
  }
}
