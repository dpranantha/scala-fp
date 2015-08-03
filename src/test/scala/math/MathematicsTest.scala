package math

import org.scalatest._
import org.scalatest.Assertions._

class MathematicsTest extends FlatSpec with Matchers {
  def fixture =
    new {
      val func = new Mathematics()
    }

  "fib" should "give the correct value" in {
    val f = fixture
    assert(fixture.func.fib(0) === 0)
    assert(fixture.func.fib(1) === 1)
    assert(fixture.func.fib(2) === 1)
    assert(fixture.func.fib(3) === 2)
    assert(fixture.func.fib(4) === 3)
    assert(fixture.func.fib(5) === 5)
    assert(fixture.func.fib(6) === 8)
  }

   "fac" should "give the correct value" in {
    val f = fixture
    assert(fixture.func.fac(0) === 1)
    assert(fixture.func.fac(1) === 1)
    assert(fixture.func.fac(2) === 2)
    assert(fixture.func.fac(3) === 6)
  }
}