package sculpt

import org.scalatest.{FreeSpec, Matchers}
import fastparse._
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.compatible.Assertion

class Spec extends FreeSpec with ChiselScalatestTester with Matchers {
  "Parser" - {
    "numbers" in {
      parse("123", Parser.num(_)) shouldBe a[fastparse.Parsed.Success[_]]
      parse("-5", Parser.num(_)) shouldBe a[fastparse.Parsed.Success[_]]
      parse("foo", Parser.num(_)) shouldBe a[fastparse.Parsed.Failure]
    }

    "call" in {
      parse("(foo 5 6 )", Parser.expr(_)).get.value shouldBe Call(
        "foo",
        Vector(Const(5), Const(6))
      )
    }
  }

  "Compatibility with Eval for:" - {
    "simple program" in {
      val pgm = """
(def main (a b)
  (let x (+ a b)
    (halt x)))
      """
      testProgram(pgm, Vector(1, 2))
    }

    "recursive multiplication" in {
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

(def main (n m) (mul n m))
        """
      testProgram(pgm, Vector(10, 17))
    }
  }

  def testProgram(
      pgm: String,
      init: Vector[Int],
      maxCycles: Int = 1000
  ): Assertion = {
    val res = parse(pgm, Parser.program(_))
    res shouldBe a[Parsed.Success[_]]
    val defs = res.get.value
    val defMap = defs.map(f => f.name -> f).toMap
    val main = defMap("main")
    val expr = Call("main", init.map(Const))
    val Num(expected) = Eval(defMap, Map(), Store(), expr)
    test(Compiler.compile(defs)) { res =>
      res.io.address.poke("hFF".U)
      res.io.readData.expect(0.S)
      for ((r, x) <- main.parameters.zip(init)) {
        res.io.address.poke(r.U)
        res.io.writeData.poke(x.S)
        res.io.write.poke(true.B)
        res.clock.step()
      }
      res.io.write.poke(false.B)
      for ((r, x) <- main.parameters.zip(init)) {
        res.io.address.poke(r.U)
        res.io.readData.expect(x.S)
        res.clock.step()
      }
      res.io.write.poke(true.B)
      res.io.address.poke("hFF".U)
      res.clock.step()
      res.io.write.poke(false.B)
      res.io.address.poke("hFF".U)
      res.io.readData.expect(1.S)
      res.clock.step(maxCycles)
      res.io.address.poke("hFF".U)
      res.io.readData.expect(0.S)
      res.io.address.poke("hFE".U)
      res.io.readData.expect(expected.S)
    }
    assert(true)
  }
}
