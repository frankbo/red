package Chapters

object Chapter2 {
  def fibonacci(first: Int, second: Int, n: Int): Int = {
    val withOutFirstTwoInputs = n - 2
    if (withOutFirstTwoInputs <= 0) {
      second
    } else {
      fibonacci(second, first + second, n - 1)
    }
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if(as.length <= 1) true
    else if (ordered(as.head, as.tail.head)) isSorted(as.tail, ordered)
    else false
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b:B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
