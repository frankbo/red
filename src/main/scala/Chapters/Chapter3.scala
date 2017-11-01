package Chapters

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Chapter3 {
  def patternMatch(l: List[Int]): Int = {
    l match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => l
    case Cons(_, xs) => xs
    case _ => l
  }

  def setHead[A](a: A, l: List[A]): List[A] = l match {
    case Cons(_, xs) => Cons(a, xs)
    case _ => l
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  // TODO is there a better implementation
  def init[A](l: List[A]): List[A] = {
    def go(li: List[A], result: List[A]): List[A] = li match {
      case Cons(x, xs) =>
        if (tail(xs) == Nil) result
        else go(xs, List.append(result, Cons(x, Nil)))
      case Nil => List()
    }

    go(l, List())
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  // TODO 3.7 and 3.8

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => y + 1)

}
