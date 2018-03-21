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
    RNG.intDouble(SimpleRNG(123)) shouldEqual((47324114, 0.179954728577286), SimpleRNG(
      256148600186669L))
  }

  "doubleInt" should "return a random tuple of double and int" in {
    RNG.doubleInt(SimpleRNG(123)) shouldEqual((0.179954728577286, 47324114), SimpleRNG(
      256148600186669L))
  }

  "double3" should "return 3 random double values" in {
    RNG.double3(SimpleRNG(123)) shouldEqual((0.022037007845938206,
      0.179954728577286,
      0.3753405185416341), SimpleRNG(
      52824481913908L))
  }

  "ints" should "return a list of random integers" in {
    RNG.ints(3)(SimpleRNG(123)) shouldEqual(List(806037626,
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
    RNG.flatMap(RNG.unit(2))(v => RNG.unit(v + 1))(SimpleRNG(123)) shouldEqual(3, SimpleRNG(
      123))
  }

  "mapWithFlatMap" should "combine return a new Random value" in {
    RNG.mapWithFlatMap(RNG.unit(2))(v => v + 1)(SimpleRNG(123)) shouldEqual(3, SimpleRNG(
      123))
  }

  "map2WithFlatMap" should "combine return a new Random value" in {
    RNG.map2WithFlatMap(RNG.int, RNG.int)((v1, v2) => v1 + v2)(SimpleRNG(123)) shouldEqual
      (-339125724, SimpleRNG(256148600186669L))
  }

  // Tests for state
  "unit" should "return a tuple with the given value and a state" in {
    State.unit(1).run(1234) shouldEqual(1, 1234)
  }

  "map" should "map return a new state with a mapped vale" in {
    State.unit(1).map(_ + 1).run(1234) shouldEqual(2, 1234)
  }

  "map2" should "map over two States and return one new mapped State" in {
    State
      .unit[Int, Int](11)
      .map2(State.unit(22))(_ + _)
      .run(123) shouldEqual(33, 123)
  }

  "flatMap" should "return a new state with a flattened and mapped value" in {
    State
      .unit[Int, Int](1)
      .flatMap(v => State.unit(v + 1))
      .run(1234) shouldEqual(2, 1234)
  }

  "sequence" should "wrap list in container element. Move wrapper from inside to outside" in {
    State
      .sequence(
        List(State.unit[Int, Int](1),
          State.unit[Int, Int](2),
          State.unit[Int, Int](3)))
      .run(123) shouldEqual
      ((List(1, 2, 3), 123))
  }

  // Tests for the candy machine
  "simulateMachine" should "unlock when a coin was insert" in {
    Machine
      .simulateMachine(List(Coin))
      .run(Machine(locked = true, 1, 2)) shouldEqual ((1, 3), Machine(locked = false, 1, 3))

  }

  it should "lock after a knob unlocked the the machine and dispence a candy" in {
    Machine
      .simulateMachine(List(Turn))
      .run(Machine(locked = false, 1, 2)) shouldEqual ((0, 2), Machine(locked = true, 0, 2))
  }

  it should "do nothing when turning the knob on a locked machine" in {
    Machine
      .simulateMachine(List(Turn))
      .run(Machine(locked = true, 1, 2)) shouldEqual ((1, 2), Machine(locked = true, 1, 2))
  }

  it should "do nothing when inserting a coin into an unlocked machine" in {
    Machine
      .simulateMachine(List(Coin))
      .run(Machine(locked = false, 1, 2)) shouldEqual ((1, 2), Machine(locked = false, 1, 2))
  }

  it should "ignore Turns when there are no candies left" in {
    Machine
      .simulateMachine(List(Turn))
      .run(Machine(locked = false, 0, 2)) shouldEqual ((0, 2), Machine(locked = false, 0, 2))
  }

  it should "ignore Coins when there are no candies left" in {
    Machine
      .simulateMachine(List(Coin))
      .run(Machine(locked = false, 0, 2)) shouldEqual ((0, 2), Machine(locked = false, 0, 2))
  }

  it should "give me two candies after inserting a Coin and Turn, insert another Coin and Turn again" in {
    Machine
      .simulateMachine(List(Coin, Turn, Coin, Turn))
      .run(Machine(locked = false, 2, 2)) shouldEqual ((0, 2), Machine(locked = false, 0, 4))
  }
}
