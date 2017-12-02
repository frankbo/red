package Chapters.Chapter2

object GettingStarted {
  def fibonacci(first: Int, second: Int, n: Int): Int = {
    val withOutFirstTwoInputs = n - 2
    if (withOutFirstTwoInputs <= 0) {
      second
    } else {
      fibonacci(second, first + second, n - 1)
    }
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    as.toList match {
      case x :: y :: _ if ordered(x, y)  => isSorted(as.tail, ordered)
      case _ :: _ :: _                   => false
      case _                             => true
    }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b:B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
