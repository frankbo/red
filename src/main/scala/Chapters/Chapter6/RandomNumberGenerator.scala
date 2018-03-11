package Chapters.Chapter6

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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v1, n1) = rng.nextInt
    (if (v1 < 0) -(v1 + 1) else v1, n1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v1, r1) = nonNegativeInt(rng)
    (v1 / (Int.MaxValue.toDouble + 1), r1)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (v1, n1) = rng.nextInt
    val (v2, n2) = double(n1)
    ((v1, v2), n2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), ran) = intDouble(rng)
    ((d, i), ran)
  }

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
        case _                    => (l, rand)
      }
    }

    go(List.empty, count, rng)
  }
}
