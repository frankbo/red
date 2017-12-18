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
    val avg = if (xs.isEmpty) None else Some(xs.sum / xs.length)
    val numerator = flatMap(Some(xs))(l => map(avg)(v => l.foldLeft(0.0)((acc, x) => acc + math.pow(x - v, 2.0))))

    map(numerator)(v => v / xs.length)
  }
}
