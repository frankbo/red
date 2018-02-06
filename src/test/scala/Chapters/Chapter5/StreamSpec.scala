package Chapters.Chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {
  "toList" should "convert an empty Stream into an empty list" in {
    Stream().toList shouldEqual List()
  }

  it should "convert a stream into a list" in {
    Stream(1, 2, 3).toList shouldEqual List(1, 2, 3)
  }
}
