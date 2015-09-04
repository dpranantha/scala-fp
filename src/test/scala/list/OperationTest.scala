package list

import org.scalatest._
import org.scalatest.Assertions._

class OperationTest extends FlatSpec with Matchers {
  def fixture =
    new {
      val op = new Operation()
      val lint = List(1, 2, 3, 4)
      val llint = List(List(1, 2), List(3, 4))
      val ld = List(1.0, 2.0, 3.0, 4.0)
      val emptylint = Nil
      val lstr = List("a", "b", "c")
      val llstr = List(List("a", "b", "c"), List("d", "e"))
      val sLeaf = Leaf(None)
      val sLeaf2 = Leaf(Some(1))
      val sBranch = Branch(Branch(Leaf(Some(9)), Leaf(None)), Branch(Leaf(Some(2)), Leaf(Some(3))))
      val sBranches = Branch(Branch(Leaf(Some(9)), Leaf(None)), Branch(Leaf(Some(2)), Branch(Leaf(Some(3)), Leaf(Some(4)))))
    }

  "removeFirst" should "remove the first element of a list" in {
    val f = fixture
    assert(fixture.op.removeHead(fixture.lint) === List(2, 3, 4))
    assert(fixture.op.removeHead(fixture.emptylint) === Nil)
  }

  "setHead" should "set the first element of a list into something else" in {
    val f = fixture
    assert(fixture.op.setHead(fixture.lint, 5) === List(5, 2, 3, 4))
    assert(fixture.op.setHead(fixture.lstr, "d") === List("d", "b", "c"))
  }

  "drop" should "drop the N elements of a list" in {
    val f = fixture
    assert(fixture.op.drop(fixture.lint, 2) === List(3, 4))
    assert(fixture.op.drop(fixture.lstr, 3) === Nil)
  }

  "dropWhile" should "drop the elements of a list while predicate is true" in {
    val f = fixture
    assert(fixture.op.dropWhile(fixture.lint)((a: Int) => a <= 2) === List(3, 4))
    assert(fixture.op.dropWhile(fixture.lstr)((a: String) => a != "c") === List("c"))
    assert(fixture.op.dropWhile(fixture.lint)((a: Int) => a <= 5) === Nil)
  }

  "init" should "drop the last element of a list" in {
    val f = fixture
    assert(fixture.op.init(fixture.lint) === List(1, 2, 3))
    assert(fixture.op.init(fixture.lstr) === List("a", "b"))
  }

  "foldLeft" should "work on list" in {
    val f = fixture
    assert(fixture.op.foldLeft(fixture.lint, 0)((x: Int, y: Int) => x + y) === 10)
    assert(fixture.op.foldLeft(fixture.lint, 1)((x: Int, y: Int) => x * y) === 24)
    assert(fixture.op.foldRight(fixture.lint, 0)((x: Int, y: Int) => x + y) === 10)
    assert(fixture.op.foldRight(fixture.lint, 1)((x: Int, y: Int) => x * y) === 24)
  }

  "mathOp" should "work on list" in {
    val f = fixture
    assert(fixture.op.sum(fixture.lint) === 10)
    assert(fixture.op.product(fixture.lint) === 24)
  }

  "reverse" should "work on list" in {
    val f = fixture
    assert(fixture.op.reverse(fixture.lint) === List(4, 3, 2, 1))
  }

  "append" should "work on list" in {
    val f = fixture
    assert(fixture.op.append(fixture.lint, 2) === List(1, 2, 3, 4, 2))
  }

  "increment" should "work on list" in {
    val f = fixture
    assert(fixture.op.increment(fixture.lint) === List(2, 3, 4, 5))
  }

  "doubleToString" should "work on list" in {
    val f = fixture
    assert(fixture.op.doubleToString(fixture.ld) === List("1.0", "2.0", "3.0", "4.0"))
  }

  "map" should "work on list" in {
    val f = fixture
    assert(fixture.op.map(fixture.lint)(x => x + 2) === List(3, 4, 5, 6))
  }
  "filter" should "work on list" in {
    val f = fixture
    assert(fixture.op.filter(fixture.lint)(x => x > 1) === List(2, 3, 4))
  }
  "flatmap" should "work on list" in {
    val f = fixture
    assert(fixture.op.flatMap(fixture.lint)(x => List(x, x + 1)) === List(1, 2, 2, 3, 3, 4, 4, 5))
  }
  "filter2" should "work on list" in {
    val f = fixture
    assert(fixture.op.filter2(fixture.lint)(x => x < 4) === List(1, 2, 3))
  }
  "add" should "work on list" in {
    val f = fixture
    assert(fixture.op.add(fixture.lint, fixture.lint) === List(2, 4, 6, 8))
    assert(fixture.op.add(fixture.lint, fixture.op.filter2(fixture.lint)(x => x < 3)) === List(2, 4, 3, 4))
  }
  "zipWith" should "work on list" in {
    val f = fixture
    assert(fixture.op.transpose(fixture.llint) === List(List(1, 3), List(2, 4)))
    assert(fixture.op.zipWith(fixture.llint)(fixture.op.sum(_)) === List(4, 6))
    assert(fixture.op.zipWith(fixture.llstr)(_.mkString(",")) === List("a,d", "b,e", "c"))
  }
  "hasSubsequence" should "work on list" in {
    val f = fixture
    assert(fixture.op.hasSubsequence(fixture.lint, List(1, 2)) === true)
    assert(fixture.op.hasSubsequence(fixture.lint, List(2, 3)) === true)
    assert(fixture.op.hasSubsequence(fixture.lint, List(4)) === true)
    assert(fixture.op.hasSubsequence(fixture.lint, List(4, 5)) === false)
    assert(fixture.op.hasSubsequence(fixture.lint, List(2, 1)) === false)
  }
  "sizeTree" should "work on tree" in {
    val f = fixture
    assert(fixture.op.size(fixture.sLeaf) === 1)
    assert(fixture.op.size(fixture.sLeaf2) === 1)
    assert(fixture.op.size(fixture.sBranches) === 9)
    assert(fixture.op.countNonEmptyLeaves(fixture.sBranches) === 4)
  }
  "max" should "work on tree" in {
    val f = fixture
    assert(fixture.op.maximum(fixture.sBranches) === 9)
  }
  "depth" should "work on tree" in {
    val f = fixture
    assert(fixture.op.depth(fixture.sBranches) === 3)
  }
  "map" should "work on tree" in {
    val f = fixture
    assert(fixture.op.mapTree(fixture.sBranch)(_ * 2) === Branch(Branch(Leaf(Some(18)), Leaf(None)), Branch(Leaf(Some(4)), Leaf(Some(6)))))
  }

}