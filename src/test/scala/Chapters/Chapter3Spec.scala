package Chapters

import org.scalatest._

class Chapter3Spec extends FlatSpec with Matchers {
  "patternMatch" should "return 3 when list has numbers from 1 to 5" in {
    Chapter3.patternMatch(List(1, 2, 3, 4, 5)) shouldEqual 3
  }

  "patternMatch" should "return first element of the list" in {
    Chapter3.patternMatch(List(22, 2, 4)) shouldEqual 22
  }

  "patternMatch" should "return sum of all elements" in {
    Chapter3.patternMatch(List(22, 22, 22)) shouldEqual 66
  }

  "patternMatch" should "return 42 when list is empty" in {
    Chapter3.patternMatch(List()) shouldEqual 42
  }

  "patternMatch" should "return 101 as a default value" in {
    // Don't know an example where the default case can show up.
  }

  "tail" should "return the list without the first element" in {
    Chapter3.tail(List(1, 2, 3, 4)) shouldEqual List(2, 3, 4)
    Chapter3.tail(List("Hello", "World")) shouldEqual List("World")
  }

  "tail" should "return the original List when there is no tail" in {
    Chapter3.tail(List(1)) shouldEqual List(1)
  }

  "setHead" should "Add an element to the first position" in {
    Chapter3.setHead(4, List(1,2,3)) shouldEqual List(4,2,3)
  }

  "drop" should "removes the first n elements" in {
    Chapter3.drop(List(1,2,3,4), 2) shouldEqual List(3,4)
  }

  "drop" should "return Nil when the list is empty" in {
    Chapter3.drop(List(), 2) shouldEqual Nil
  }

  "drop" should "return the original list when nothing should be dropped" in {
    Chapter3.drop(List(1,2, 3), 0) shouldEqual List(1,2,3)
  }

  "dropWhile" should "remove elements as long as a pattern matches" in {
    Chapter3.dropWhile(List(0, 0, 3, 0, 4))(isZero) shouldEqual List(3,0,4)
  }

  "dropWhile" should "return an empty list as a default case" in {
    Chapter3.dropWhile(List())(isZero) shouldEqual Nil
  }

  "init" should "return the list without it's last element" in {
    Chapter3.init(List(1, 2, 3)) shouldEqual List(1, 2)
  }

  def isZero(a: Int): Boolean = a == 0
}