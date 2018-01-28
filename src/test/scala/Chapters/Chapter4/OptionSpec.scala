package Chapters.Chapter4

import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers {
  "map" should "map over a value when a valid Option was passed" in {
    Option.map(Some(2))(_ + 1) shouldEqual Some(3)
  }

  it should "return None if nothing was passed" in {
    Option.map[Int, Int](None)(_ + 1) shouldEqual None
  }

  "flatMap" should "return None if nothing was passed" in {
    Option.flatMap(None)(identity) shouldEqual None
  }

  it should "flatten the passed structure when a valid Option was passed" in {
    Option.flatMap(Some(Some(2)))(v => Option.map(v)(_ + 1)) shouldEqual Some(3)
  }

  "getOrElse" should "return the value if a valid Option was passed" in {
    Option.getOrElse(Some(33))(22) shouldEqual 33
  }

  it should "return the default value when nothing valid was passed" in {
    Option.getOrElse(None)(22) shouldEqual 22
  }

  "orElse" should "return the value if a valid Option was passed" in {
    Option.orElse(Some(33))(Some(22)) shouldEqual Some(33)
  }

  it should "return the default value when nothing valid was passed" in {
    Option.orElse(None)(Some(22)) shouldEqual Some(22)
  }

  "filter" should "return Some value when filter condition was truthy" in {
    Option.filter(Some(33))(_ > 20) shouldEqual Some(33)
  }

  it should "return None when filter function returned false" in {
    Option.filter(Some(1))(_ > 2) shouldEqual None
  }

  it should "return None when None was passed" in {
    Option.filter[Int, Int](None)(_ > 2) shouldEqual None
  }

  "variance" should "return the variance of given sequence" in {
    Option.variance(Seq(8.0, 7.0, 9.0, 10.0, 6.0)) shouldEqual Some(2)
  }

  it should "return None for an empty Sequence" in {
    Option.variance(Seq()) shouldEqual None
  }

  "map2" should "return an Some value when 2 params were passed to a function" in {
    Option.map2(Some(2), Some(2))((a: Int, b: Int) => a + b) shouldEqual Some(4)
  }

  it should "return None when one of both parameters is None" in {
    Option.map2(Some(2), None)((a, b) => a + b) shouldEqual None
    Option.map2(None, Some(2))((a: Int, b: Int) => a + b) shouldEqual None
    Option.map2(None, None)((a: Int, b: Int) => a + b) shouldEqual None
  }

  "sequence" should "return an Some with a list included" in {
    Option.sequence(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
  }

  it should "return None when one of the element is Nonen" in {
    Option.sequence(List(Some(1), None, Some(3))) shouldEqual None
  }
}
