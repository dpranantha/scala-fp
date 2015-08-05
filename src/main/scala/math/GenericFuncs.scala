package math

class GenericFuncs {
  def findFirst[A](collection: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= collection.length) -1
      else if(p(collection(n))) n
      else loop(n+1)
    }
    loop(0)
  }
  def isSorted[A](collection: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n+1 >= collection.length - 1) ordered(collection(n),collection(n+1))
      else if(ordered(collection(n), collection(n+1))) loop(n+1)
      else false
    }
    if(collection.length <= 1) true
    else loop(0)
  }
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a: A, b: B) => f(a)(b)
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}