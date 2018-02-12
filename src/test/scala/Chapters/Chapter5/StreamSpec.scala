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
    Stream(1, 2, 3, 4, 5, 6).take(2).toList shouldEqual Stream(1, 2).toList
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
    Stream(1, 2, 3, 4).forAll((v) => v < 2) shouldEqual false
  }

  "taleWhile2" should "take elements from a stream as long as the passed function is truthy" in {
    Stream(1, 2, 3, 4).takeWhile2((v) => v <= 2).toList shouldEqual Stream(1, 2).toList
  }

  "headOption2" should "return the head wrapped in a Some when available" in {
    Stream(1, 2, 3).headOption2 shouldEqual Some(1)
  }

  it should "return None when no head is available" in {
    Empty.headOption2 shouldEqual None
  }

  "map" should "call a function on every element on the stream lazy" in {
    Stream(1, 2, 3).map(v => v + 1).toList shouldEqual Stream(2, 3, 4).toList
  }

  "filter" should "only return the values that match the function that filters" in {
    Stream(1, 2, 3).filter(v => v % 2 == 1).toList shouldEqual Stream(1, 3).toList
  }

  it should "return an empty list when no element are filtered out" in {
    Stream(1, 2, 3).filter(v => v > 6).toList shouldEqual Empty.toList
  }

  "append" should "combine two Streams to one" in {
    Stream.append(Stream(1, 2, 3), Stream(4, 5, 6)).toList shouldEqual Stream(1, 2, 3, 4, 5, 6).toList
  }

  "flatMap" should "go over every element in a stream a call a function + flatten the otucome" in {
    Stream(1, 2, 3).flatMap(v => Stream.cons(v + 1, Stream.empty)).toList shouldEqual Stream(2, 3, 4).toList
  }

  "constant" should "create an endless amount of some given value" in {
    Stream.constant(33).take(4).toList shouldEqual Stream(33, 33, 33, 33).toList
  }

  "from" should "start from a given value and counts infinite time + 1" in {
    Stream.from(2).take(3).toList shouldEqual Stream(2, 3, 4).toList
  }

  "fibs" should "calculate an infinite fibonacci series" in {
    Stream.fibs().take(7).toList shouldEqual Stream(0, 1, 1, 2, 3, 5, 8).toList
  }

  "fibs2" should "calculate an infinite fibonacci series" in {
    Stream.fibs2().take(7).toList shouldEqual Stream(0, 1, 1, 2, 3, 5, 8).toList
  }

  "unfold" should "create the next value and the next state for a stream" in {
    Stream.unfold(4)((v) => Some(v, v + 1)).take(5).toList shouldEqual Stream(4, 5, 6, 7, 8).toList
  }

  "from2" should "start from a given value and counts infinite time + 1" in {
    Stream.from2(4).take(5).toList shouldEqual Stream(4, 5, 6, 7, 8).toList
  }

  "constant2" should "create an endless amount of some given value" in {
    Stream.constant2(33).take(4).toList shouldEqual Stream(33, 33, 33, 33).toList
  }

  "ones2" should "return an infinate amount of the value 1" in {
    Stream.ones2.take(5).toList shouldEqual Stream(1, 1, 1, 1, 1).toList
  }
}
