package Chapters.Chapter5

sealed trait Stream[+A] {
  def toList: List[A]

  def take(n: Int): Stream[A]

  def drop(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A]

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
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

  def flatMap[B](p: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => Stream.append(p(h), t))

  def filter[B](p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

  def tails: Stream[Stream[A]] =
    Stream.append(Stream.unfold(this) {
      case Cons(h, t) => Some((Cons(h, t), t()))
      case Empty      => None
    }, Stream(Empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    Stream.append(Stream.unfold(this) {
      case Cons(h, t) => Some((Cons(h, t).foldRight(z)(f), t()))
      case Empty      => None
    }, Stream(z))
}

case object Empty extends Stream[Nothing] {
  override def toList = List()

  override def take(n: Int) = Stream()

  override def drop(n: Int) = Stream()

  override def takeWhile(p: Nothing => Boolean) = Stream()
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList = h() :: t().toList

  override def take(n: Int) =
    if (n == 0) Empty else Cons(h, () => t().take(n - 1))

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

  def append[A](s1: => Stream[A], s2: => Stream[A]): Stream[A] =
    s1.foldRight(s2)((h, t) => cons(h, t))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def constant2[A](a: A): Stream[A] = unfold(a)((v) => Some(v, v))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def from2(n: Int): Stream[Int] = unfold(n)((v) => Some(v, v + 1))

  val ones2: Stream[Int] = unfold(1)((v) => Some(v, v))

  def fibs(): Stream[Int] = {
    def go(v1: Int, v2: Int): Stream[Int] = {
      val next = v1 + v2
      cons(v1, cons(v2, go(next, next + v2)))
    }

    go(0, 1)
  }

  def fibs2(): Stream[Int] =
    unfold((0, 1))((v) => Some(v._1, (v._2, v._1 + v._2)))

  def takeUnfold[A](s: Stream[A])(n: Int): Stream[A] =
    unfold((n, s)) {
      case (counter, Cons(h, t)) if counter > 0 =>
        Some((h(), (counter - 1, t())))
      case _ => None
    }

  def takeWhileUnfold[A](s: Stream[A])(f: A => Boolean): Stream[A] =
    unfold(s) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _                    => None
    }

  def zipWithUnfold[A](a: Stream[A])(b: Stream[A])(
      f: ((A, A)) => A): Stream[A] =
    unfold((a, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _                            => None
    }

  def startsWith[A](s1: Stream[A])(s2: Stream[A]): Boolean =
    zipAll(s1)(s2)
      .map {
        case (Some(v1), Some(v2)) => v1 == v2
        case (Some(v1), None)     => true
        case _                    => false
      }
      .forAll(identity)

  def zipAll[A, B](s1: Stream[A])(
      s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((s1, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case _                     => None
    }

  def mapUnfold[A, B](s: Stream[A])(f: A => B): Stream[B] =
    unfold(s) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty      => None
    }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((value, state)) => cons(value, unfold(state)(f))
    case None                 => empty
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
