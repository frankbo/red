package Chapters.Chapter6

import Chapters.Chapter6.RNG.{sequence, unit}

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (v1, n1) = ra(rng)
      val (v2, n2) = rb(n1)
      (f(v1, v2), n2)
    }

  // Map2 the Perri way
  def map2Perri[A, B, C]: Rand[A] => Rand[B] => ((A, B) => C) => Rand[C] =
    ra =>
      rb =>
        f =>
          rng => {
            val (v1, n1) = ra(rng)
            val (v2, n2) = rb(n1)
            (f(v1, v2), n2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v1, n1) = rng.nextInt
    (if (v1 < 0) -(v1 + 1) else v1, n1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v1, r1) = nonNegativeInt(rng)
    (v1 / (Int.MaxValue.toDouble + 1), r1)
  }

  def doubleWithMap(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(v => v / (Int.MaxValue.toDouble + 1))(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (v1, n1) = rng.nextInt
    val (v2, n2) = double(n1)
    ((v1, v2), n2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), ran) = intDouble(rng)
    ((d, i), ran)
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, n1) = double(rng)
    val (d2, n2) = double(n1)
    val (d3, n3) = double(n2)
    ((d1, d2, d3), n3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(l: List[Int], c: Int, rand: RNG): (List[Int], RNG) = {
      val (v, ng) = RNG.nonNegativeInt(rand)
      l match {
        case xs if c > 0 => go(v :: xs, c - 1, ng)
        case _           => (l, rand)
      }
    }

    go(List.empty, count, rng)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil     => unit(List.empty)
    case x :: xs => RNG.map2(x, sequence(xs))((v, l) => v :: l)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (v, r) = f(rng)
    g(v)(r)
  }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(v => unit(f(v)))

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C): Rand[C] =
    flatMap(ra)(v1 => map(rb)(v2 => f(v1, v2)))

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    State(s => {
      val (a, ns) = this.run(s)
      (f(a), ns)
    })
  }

  def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(ns => {
      val (v1, n1) = this.run(ns)
      val (v2, n2) = s.run(n1)
      (f(v1, v2), n2)
    })
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (v, ns) = this.run(s)
      f(v).run(ns)
    })
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = l match {
    case Nil     => unit(List.empty)
    case x :: xs => x.map2(sequence(xs))((v, nl) => v :: nl)
  }
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs match {
      case x :: xs =>
        State(s => {
          x match {
            case Coin if s.candies > 0 && s.locked => {
              ((1, s.coins + 1),
               Machine(locked = false, s.candies, s.coins + 1))
            }
            case Turn if s.candies > 0 && !s.locked => {
              ((s.candies - 1, s.coins),
               Machine(locked = true, s.candies - 1, s.coins))
            }
            case _ => ((s.candies, s.coins), s)
          }
        })
      case Nil     => State(s => ((s.candies, s.coins), s))
    }
}
