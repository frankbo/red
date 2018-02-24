package Chapters.Chapter6


import org.scalatest.{FlatSpec, Matchers}

class RandomNumberGeneratorSpec extends FlatSpec with Matchers {
  "nonNegativeInt" should "always return a positive number" in {
    val rng = SimpleRNG(123)
    val (_, rng2) = rng.nextInt
    rng.nonNegativeInt(rng2)._1 should be > 0
  }

  "double" should "always a random number between 0 and 1 as Double" in {
    val rng = SimpleRNG(123)
    val (_, rng2) = rng.nextInt
    rng.double(rng2)._1 should be > 0.0
    rng.double(rng2)._1 should be < 1.0
  }
}
