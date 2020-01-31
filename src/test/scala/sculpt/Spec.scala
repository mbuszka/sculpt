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

    "memory usage" in {
      val pgm = """
(def gen (n acc)
  (match n
    (0 (sum acc 0))
    (else
      (let xs (Cons n acc)
        (let n (- n 1)
          (gen n xs))))))

(def sum (lst acc)
  (match lst
    (Nil (halt acc))
    (Cons x lst
      (let acc (+ acc x)
        (sum lst acc)))))
      
(def main (n)
  (let nil (Nil)
    (gen n nil)))
      """
      testProgram(pgm, Vector(15))
    }
  }

  def testProgram(
      pgm: String,
      init: Vector[Int],
      maxCycles: Int = 1000,
      verbose: Boolean = false
  ): Assertion = {
    val res = parse(pgm, Parser.program(_))
    res shouldBe a[Parsed.Success[_]]
    val defs = res.get.value
    if (verbose) pprint.pprintln(defs, height = 100)
    val defMap = defs.map(f => f.name -> f).toMap
    val main = defMap("main")
    val expr = Call("main", init.map(Const))
    val Num(expected) = Eval(defMap, Map(), Store(), expr)
    test(Pipeline(defs, verbose=verbose)) { res =>
      def write(addr: UInt, value: SInt) = {
        res.io.address.poke(addr)
        res.io.writeData.poke(value)
        res.io.write.poke(true.B)
        res.clock.step()
        res.io.write.poke(false.B)
      }
      def check(addr: UInt, expected: SInt) = {
        res.io.address.poke(addr)
        res.io.readData.expect(expected)  
      }
      check("hFF".U, Status.Idle.S) // check that processor is initialized idle
      // load data to registers
      for ((r, x) <- main.parameters.zip(init)) {
        write(r.U, x.S)
      }
      // check that data was correctly loaded
      for ((r, x) <- main.parameters.zip(init)) {
        check(r.U, x.S)
      }
      
      write("hFF".U, 0.S) // begin execution
      check("hFF".U, Status.Busy.S) // check that processor is busy
      
      res.clock.step(maxCycles) // let it run for a while
      check("hFF".U, Status.Idle.S) // check that it finished
      
      check("hFE".U, expected.S) // check the result w.r.t. eval
    }
    assert(true)
  }
}
