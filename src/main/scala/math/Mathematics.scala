package math

class Mathematics {
  def fib(n: Int): Int = {
    def go(n: Int, n_1: Int, n_2: Int): Int = {
      if (n == 0) n_2 else go(n-1, n_1 + n_2, n_1)
    }
    go(n, 1, 0);
  }
  def fac(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go(n, 1)
  }
}