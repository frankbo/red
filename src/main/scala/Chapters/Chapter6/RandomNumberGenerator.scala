package Chapters.Chapter6

trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  // Todo Catch Int.MinValue error and find out how to write a test for it.
  override def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v1, n1) = rng.nextInt
    if (v1 >= 0) (v1, n1) else nonNegativeInt(n1)
  }
}