package Chapters.Chapter4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e)  => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap[B, EE >: E](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_)  => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (Right(v1), Right(v2)) => Right(f(v1, v2))
      case (Right(_), Left(v2))   => Left(v2)
      case (Left(v), _)           => Left(v)
    }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil    => Right(Nil)
    case h :: t => h.flatMap(rightHead => sequence(t).map(rightHead :: _))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t =>
        for {
          hh <- f(h)
          l <- traverse(t)(f)
        } yield hh :: l
    }
}

/* 4.8
Collecting errors in a list might be the best solution
Either[List[E], B]
 */
