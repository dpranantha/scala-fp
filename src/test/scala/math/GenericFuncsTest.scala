package math

import org.scalatest._
import org.scalatest.Assertions._

class GenericTest extends FlatSpec with Matchers {
  def fixture =
    new {
      val func = new GenericFuncs()
      val collectionA = Array(1, 2, 3, 4, 5)
      val collectionB = Array(2, 1, 1, 3, 0)
      val collectionC = Array()
      val collectionD = Array(1)
    }

  "findFirst" should "give the correct index" in {
    val f = fixture
    assert(fixture.func.findFirst(fixture.collectionA, (x:Int) => x == 3) === 2)
    assert(fixture.func.findFirst(fixture.collectionB, (x:Int) => x == 3) === 3)
    assert(fixture.func.findFirst(fixture.collectionC, (x:Int) => x == 3) === -1)
  }

   "isSorted" should "give the correct predicate" in {
    val f = fixture
    assert(fixture.func.isSorted(fixture.collectionA, (x:Int, y:Int) => x <= y ) === true)
    assert(fixture.func.isSorted(fixture.collectionB, (x:Int, y:Int) => x <= y ) === false)
    assert(fixture.func.isSorted(fixture.collectionC, (x:Int, y:Int) => x <= y ) === true)
  }
}