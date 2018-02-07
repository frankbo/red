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
    Stream(1,2,3,4,5,6).take(1).toList shouldEqual Stream(1,2).toList
  }

  "drop" should "return an empty stream when there is nothing to drop" in {
    Stream().drop(1) shouldEqual Stream()
  }

  it should "return the remaining stream when the n elements are dropped" in {
    Stream(1, 2, 3, 4).drop(1).toList shouldEqual Stream(3, 4).toList // Why is toList relevant here?
  }
}