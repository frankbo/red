package Chapters.Chapter5

sealed trait Stream[+A] {
  def toList: List[A]

  def take(n: Int): Stream[A]

  def drop(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A]

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((next, prev) => p(next) && prev)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((next, prev) =>
      if (p(next)) Stream.cons(next, prev) else Empty)

  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](p: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => Stream.cons(p(h), t))

}

case object Empty extends Stream[Nothing] {
  override def toList = List()

  override def take(n: Int) = Stream()

  override def drop(n: Int) = Stream()

  override def takeWhile(p: Nothing => Boolean) = Stream()
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList = h() :: t().toList

  override def take(n: Int) = if (n == 0) Empty else Cons(h, () => t().take(n - 1))

  override def drop(n: Int) =
    if (n == 0)
      Cons(h, t)
    else if (n == 1) t()
    else t().drop(n - 1)

  override def takeWhile(p: A => Boolean) =
    if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
