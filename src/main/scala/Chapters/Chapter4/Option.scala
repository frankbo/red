package Chapters.Chapter4

sealed trait Option[+A]

case class Some[+A](a: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def map[A, B](value: Option[A])(f: A => B): Option[B] = value match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[A, B](value: Option[A])(f: A => Option[B]): Option[B] = value match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[A, B >: A](value: Option[A])(default: => B): B = value match {
    case None => default
    case Some(a) => a
  }

  def orElse[A, B >: A](value: Option[A])(ob: => Option[B]): Option[B] = value match {
    case None => ob
    case v => v
  }

  def filter[A, B >: A](value: Option[A])(f: A => Boolean): Option[A] = value match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    def avg(l: Seq[Double]) =
      if (l.isEmpty) None else Some(l.sum / l.length)

    flatMap(avg(xs))(x => avg(xs.map(v => math.pow(x - v, 2.0))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }
}
