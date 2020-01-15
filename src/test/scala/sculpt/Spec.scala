package sculpt

import org.scalatest.{WordSpec, Matchers}
import fastparse._
import chisel3._
import chisel3.util._
import chiseltest._

class Spec extends WordSpec with ChiselScalatestTester with Matchers {
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

(def main (ignore) (mul 10 17))
        """
      val res = parse(pgm, Parser.program(_)).get.value
      val defs = res.map(f => f.name -> f).toMap
      pprint.pprintln(defs)
      val exp = parse("(mul 3 2)", Parser.expr(_)).get.value
      val v = Eval(defs, Map(), Store(), exp)
      pprint.pprintln(v)
      test(Compiler.compile(res)) {
        r => 
          for (_ <- 1.until(100)) {
            println(r.io.status.peek())
            r.clock.step()
          }
        println(r.io.status.peek())
        println(r.io.value.peek())
      }
    }

    "test codegen" in {
      val pgm =
      """
(def main (ignore)
  (let x (+ 3 4)
    (halt x)))
      """
      val defs = parse(pgm, Parser.program(_)).get.value
      println(Driver.emit(() => Compiler.compile(defs)))
      test(Compiler.compile(defs)) {
        r => 
          r.clock.step(10)
          println(r.io.value.peek())
      }
    }
  }
}
