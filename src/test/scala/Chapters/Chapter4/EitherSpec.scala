package Chapters.Chapter4

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {
  "map" should "map over a value when a right valid was passed" in {
    Right(2).map(_ + 1) shouldEqual Right(3)
  }

  it should "return Left when an incorrect value was passed" in {
    Left("Some error").map(_ => 1 + 1) shouldEqual Left("Some error")
  }

  "flatMap" should "return call a function on a right element" in {
    Right(2).flatMap(v => Right(v + 1)) shouldEqual Right(3)
  }

  it should "return return left element with an error" in {
    Left("Some error").flatMap(_ => Right(1 + 1)) shouldEqual Left("Some error")
  }

  "orElse" should "return the right case with correct input" in {
    Right(22).orElse(Right(33)) shouldEqual Right(22)
    Right(22).orElse(Left("Just wrong")) shouldEqual Right(22)
  }

  it should "return the fallback when an incorrect value was passed" in {
    Left("Something went wrong").orElse(Right(33)) shouldEqual Right(33)
    Left("Something went wrong").orElse(Left("Just wrong")) shouldEqual Left("Just wrong")
  }

  "map2" should "return result of passed function when two valid values were passed to function" in {
    Right(2).map2(Right(4))((v1, v2) => v1 + v2) shouldEqual Right(6)
  }

  it should "return a Left option when current value is wrong" in {
    Left("Some error").map2(Right(4))((_, _) => 1 + 2) shouldEqual Left("Some error")
    Left("Some error").map2(Left("Some other error"))((_, _) => 1 + 2) shouldEqual Left("Some error")
  }

  it should "return the error of the second Either when the current element is correct" in {
    Right(22).map2(Left("Some error"))((_, _) => 1 + 2) shouldEqual Left("Some error")
  }
}
