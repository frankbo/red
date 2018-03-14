package Chapters.Chapter6

import org.scalatest.{FlatSpec, Matchers}

class RandomNumberGeneratorSpec extends FlatSpec with Matchers {
  "nonNegativeInt" should "always return a positive number" in {
    RNG.nonNegativeInt(SimpleRNG(123))._1 should be > 0
  }

  "double" should "always a random number between 0 and 1 as Double" in {
    RNG.double(SimpleRNG(123))._1 should (be > 0.0 and be < 1.0)
  }

  "doubleWithMap" should "always a random number between 0 and 1 as Double" in {
    RNG.doubleWithMap(SimpleRNG(123))._1 should (be > 0.0 and be < 1.0)
  }

  "intDouble" should "return a random tuple of int and double" in {
    RNG.intDouble(SimpleRNG(123)) shouldEqual ((47324114, 0.179954728577286), SimpleRNG(
      256148600186669L))
  }

  "doubleInt" should "return a random tuple of double and int" in {
    RNG.doubleInt(SimpleRNG(123)) shouldEqual ((0.179954728577286, 47324114), SimpleRNG(
      256148600186669L))
  }

  "double3" should "return 3 random double values" in {
    RNG.double3(SimpleRNG(123)) shouldEqual ((0.022037007845938206,
                                              0.179954728577286,
                                              0.3753405185416341), SimpleRNG(
      52824481913908L))
  }

  "ints" should "return a list of random integers" in {
    RNG.ints(3)(SimpleRNG(123)) shouldEqual (List(806037626,
                                                  386449837,
                                                  47324114), SimpleRNG(
      52824481913908L))
  }

  "map2" should "combine two the map results to results" in {
    RNG.map2(RNG.int, RNG.int)((v1, v2) => v1 + v2)(SimpleRNG(123)) shouldEqual
      (-339125724, SimpleRNG(256148600186669L))
  }

  "map2Perri" should "combine two the map results to results" in {
    RNG.map2Perri(RNG.int)(RNG.int)((v1, v2) => v1 + v2)(SimpleRNG(123)) shouldEqual
      (-339125724, SimpleRNG(256148600186669L))
  }

  "sequence" should "transform a list of elements to one element that contains a list" in {
    RNG.sequence(List(RNG.int, RNG.int, RNG.int))(SimpleRNG(123)) shouldEqual
      (List(47324114, -386449838, 806037626), SimpleRNG(52824481913908L))
  }

  "flatMap" should "flatten and map a Rand value" in {
    RNG.flatMap(RNG.unit(2))(v => RNG.unit(v + 1))(SimpleRNG(123)) shouldEqual (3, SimpleRNG(123))
  }
}
