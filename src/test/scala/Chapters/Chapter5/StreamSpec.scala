package Chapters.Chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {
  "toList" should "convert an empty Stream into an empty list" in {
    Stream().toList shouldEqual List()
  }

  it should "convert a stream into a list" in {
    Stream(1, 2, 3).toList shouldEqual List(1, 2, 3)
  }

  "take" should "return an empty stream when take can not get anything" in {
    Stream().take(1) shouldEqual Stream()
  }

  it should "return the first n elements from the stream" in {
    Stream(1,2,3,4,5,6).take(2).toList shouldEqual Stream(1,2).toList
  }

  "drop" should "return an empty stream when there is nothing to drop" in {
    Stream().drop(1) shouldEqual Stream()
  }

  it should "return the whole Stream when 0 elements should be dropped" in {
    Stream(1, 2, 3, 4).drop(0).toList shouldEqual Stream(1, 2, 3, 4).toList
  }

  it should "return the remaining stream when the n elements are dropped" in {
    Stream(1, 2, 3, 4).drop(1).toList shouldEqual Stream(2, 3, 4).toList
  }

  "takeWhile" should "return values until functional call is truthy" in {
    Stream(1, 2, 3, 4, 5).takeWhile(v => v < 4).toList shouldEqual Stream(1, 2, 3).toList
  }

  it should "return an empty stream when the list is empty" in {
    Stream().takeWhile(_ => true) shouldEqual Stream()
  }

  "forAll" should "if all elements in a Stream match a function" in {
    Stream(1, 2, 3, 4).forAll((v) => v < 5) shouldEqual true
  }

  it should "return false when NOT all elements match a given function" in {
    Stream(1, 2, 3, 4).forAll((v) => v < 2 ) shouldEqual false
  }
}
