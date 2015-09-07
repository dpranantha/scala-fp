package list
/**
 * @author dpranantha
 */
class Operation {
  def removeHead[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h :: t => t
  }

  def setHead[A](xs: List[A], a: A): List[A] = xs match {
    case Nil => Nil
    case h :: t => a :: t
  }

  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil => Nil
    case h :: t if n > 0 => drop(t, n - 1)
    case _ => xs
  }

  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case h :: t if f(h) => dropWhile(t)(f)
    case _ => xs
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h :: Nil => Nil
    case h :: t => h :: init(t)
  }

  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil => z
    case y :: ys => foldRight(ys, f(y, z))(f)
  }

  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
    case y :: ys => foldLeft(ys, f(z, y))(f)
    case Nil => z
  }

  def sum(xs: List[Int]): Int = foldLeft(xs, 0)((a: Int, b: Int) => a + b)
  def product(xs: List[Int]): Int = foldLeft(xs, 1)((a: Int, b: Int) => a * b)
  def reverse[A](xs: List[A]): List[A] = {
    foldLeft(xs, Nil: List[A]) {
      (t, h) => h :: t
    }
  }
  def append[A](xs: List[A], a: A): List[A] = foldLeft(xs, Nil: List[A]) { (_, _) => xs :+ a }
  def increment(xs: List[Int]): List[Int] = {
    foldLeft(xs, Nil: List[Int]) {
      (t, h) => t :+ (h + 1)
    }
  }
  def doubleToString(xs: List[Double]): List[String] = {
    foldLeft(xs, Nil: List[String]) {
      (t, h) => t :+ h.toString()
    }
  }
  def map[A, B](xs: List[A])(f: A => B): List[B] = {
    foldLeft(xs, Nil: List[B]) {
      (t, h) => t :+ f(h)
    }
  }
  def filter[A](xs: List[A])(f: A => Boolean): List[A] = {
    foldLeft(xs, Nil: List[A]) {
      (t, h) => if (f(h)) t :+ h else t
    }
  }
  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = {
    foldLeft(xs, Nil: List[B]) {
      (t, h) => t ::: f(h)
    }
  }
  def filter2[A](xs: List[A])(f: A => Boolean): List[A] = {
    flatMap(xs)(x => if (f(x)) List(x) else Nil)
  }
  def add(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x :: xt, y :: yt) => x + y :: add(xt, yt)
  }
  def transpose[A](xs: List[List[A]]): List[List[A]] = {
    filter2(xs)(b => !b.isEmpty) match {
      case Nil => Nil
      case ys => map(ys)(y => y.head) :: transpose(map(ys)(y => y.tail))
    }
  }
  def zipWith[A, B](xs: List[List[A]])(f: List[A] => B): List[B] = {
    map(transpose(xs))(f);
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, Nil) => true
    case (_, Nil) => true
    case (Nil, _) => false
    case (x :: xs, y :: ys) => if (x == y) hasSubsequence(xs, ys) else hasSubsequence(xs, y :: ys)
  }
  def size[A](ts: SimpleTree[A]): Int = ts match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
  def maximum(ts: SimpleTree[Int]): Int = ts match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  def depth[A](ts: SimpleTree[A]): Int = ts match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
  def mapTree[A, B](ts: SimpleTree[A])(f: A => B): SimpleTree[B] = ts match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(mapTree(l)(f), mapTree(r)(f))
  }
  def foldLeft[A, B](ts: SimpleTree[A], z: B)(f: (B, B) => B)(g: Leaf[A] => B): B = ts match {
    case Leaf(v) => f(g(Leaf(v)),z)
    case Branch(l,r) => f(foldLeft(l,z)(f)(g),foldLeft(r,z)(f)(g))
  }
  def sizeTree[A](ts: SimpleTree[A]):Int = foldLeft(ts,1)(_ + _)((x: Leaf[A]) => 1) 
  def depthTree[A](ts: SimpleTree[A]): Int = foldLeft(ts,0)((a,b) => (a max b) + 1)((x: Leaf[A]) => 0)
  def maximumTree(ts: SimpleTree[Int]):Int = foldLeft(ts,-1)(_ max _)((x: Leaf[Int]) => x.value)
}