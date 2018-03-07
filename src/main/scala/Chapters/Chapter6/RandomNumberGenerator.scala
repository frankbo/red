package Chapters.Chapter6

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
}