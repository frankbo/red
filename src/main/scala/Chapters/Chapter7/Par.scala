package Chapters.Chapter7

case class Par[A](run: () => A)

object Par {
  def unit[A](a: A): Par[A] = Par(() => a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = a.run()

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    unit(f(run(a), run(b)))

  def fork[A](a: => Par[A]): Par[A] = a()
}
