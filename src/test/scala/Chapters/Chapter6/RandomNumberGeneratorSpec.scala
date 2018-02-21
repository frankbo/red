package Chapters.Chapter6


import org.scalatest.{FlatSpec, Matchers}

class RandomNumberGeneratorSpec extends FlatSpec with Matchers {
    "nonNegativeInt" should "always return a positive number" in {
      val rng = SimpleRNG(123)
      val (_, rng2) = rng.nextInt
      rng.nonNegativeInt(rng2)._1 should be > 0
    }
}
