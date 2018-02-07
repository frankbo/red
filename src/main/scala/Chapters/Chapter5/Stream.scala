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
}
case object Empty extends Stream[Nothing] {
  override def toList = List()
  override def take(n: Int) = Stream()
  override def drop(n: Int) = Stream()
  override def takeWhile(p: Nothing => Boolean) = Stream()
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList = h() :: t().toList
  override def take(n: Int) = if (n == 0) Stream(h()) else Cons(h, () => t().take(n - 1))
  override def drop(n: Int) = if (n == 1) t() else t().drop(n - 1)
  override def takeWhile(p: A => Boolean) = {
    val nextHead = t().headOption.map(p)
    if (p(h()) && nextHead.contains(true)) Cons(h, () => t().takeWhile(p))
    else Stream(h())
  }
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
