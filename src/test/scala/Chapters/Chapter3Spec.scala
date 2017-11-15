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
    Chapter3.tail(List(1)) shouldEqual List()
  }

  "setHead" should "Add an element to the first position" in {
    Chapter3.setHead(4, List(1, 2, 3)) shouldEqual List(4, 2, 3)
  }

  "drop" should "remove the first n elements" in {
    Chapter3.drop(List(1, 2, 3, 4), 2) shouldEqual List(3, 4)
  }

  "drop" should "remove the first element from a one element list" in {
    Chapter3.drop(List(1), 1) shouldEqual Nil
  }

  "drop" should "return Nil when the list is empty" in {
    Chapter3.drop(List(), 2) shouldEqual Nil
  }

  "drop" should "return the original list when nothing should be dropped" in {
    Chapter3.drop(List(1, 2, 3), 0) shouldEqual List(1, 2, 3)
  }

  "dropWhile" should "remove elements as long as a pattern matches" in {
    Chapter3.dropWhile(List(0, 0, 3, 0, 4))(isZero) shouldEqual List(3, 0, 4)
  }

  "dropWhile" should "return an empty list as a default case" in {
    Chapter3.dropWhile(List())(isZero) shouldEqual Nil
  }

  "init" should "return the list without it's last element" in {
    Chapter3.init(List(1, 2, 3)) shouldEqual List(1, 2)
  }

  "length" should "return the length of a list" in {
    Chapter3.lengthR(List(1, 22, 3, 44, 5, 66)) shouldEqual 6
  }

  "foldLeft" should "return the sum of a list" in {
    Chapter3.foldLeft(List(1, 22, 3, 4, 5), 0)(_ + _) shouldEqual 35
  }

  "sumL" should "return the sum of a list" in {
    Chapter3.sumL(List(1, 22, 3, 4, 5)) shouldEqual 35
  }

  "prodL" should "return the product of" in {
    Chapter3.prodL(List(1.0, 2.0, 2.0, 4.0)) shouldEqual 16.0
  }

  "reverse" should "revert the passed list" in {
    Chapter3.reverse(List(1, 2, 3)) shouldEqual List(3, 2, 1)
  }

  "appendWithFold" should "append one list to another" in {
    Chapter3.appendWithFold(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(1, 2, 3, 4, 5, 6)
  }

  "flatten" should "flatten lists of lists to own list" in {
    Chapter3.flatten(List(List(1, 2, 3), List(1, 2, 3))) shouldEqual List(1, 2, 3, 1, 2, 3)
  }

  "addOneToEveryElement" should "add one on every element in the list" in {
    Chapter3.addOneToEveryElement(List(-1, -1, 6)) shouldEqual List(0, 0, 7)
  }

  "doubleToString" should "transform every double to string in a list" in {
    Chapter3.doubleToString(List(1.1, 2.2, 3.3, 4.4)) shouldEqual List("1.1", "2.2", "3.3", "4.4")
  }

  "map" should "modify every element in a list" in {
    Chapter3.map(List(1, 2, 3))(addOne) shouldEqual List(2, 3, 4)
  }

  "filter" should "filter a list based on a passed function" in {
    Chapter3.filter(List(1, 2, 0, 0, 33))(notZero) shouldEqual List(1, 2, 33)
  }

  "flatMap" should "flatten and map a list of elements" in {
    Chapter3.flatMap(List(1, 2, 3))(i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3)
  }

  "flatMapFilter" should "filter a list based on callback function" in {
    Chapter3.flatMapFilter(List(1, 2, 0, 0, 3))(notZero) shouldEqual List(1, 2, 3)
  }

  "zipWith" should "combine two list through addition" in {
    Chapter3.zipWith(List(1, 2, 3), List(1, 2, 3), addElements) shouldEqual List(2, 4, 6)
    Chapter3.zipWith(List(1, 2, 3), List(1, 2, 3, 22), addElements) shouldEqual List(2, 4, 6, 22)
    Chapter3.zipWith(List(1, 2, 3, 35), List(1, 2, 3), addElements) shouldEqual List(2, 4, 6, 35)
  }

  def addElements(a: Int)(b: Int): Int = a + b

  def addOne(a: Int): Int = a + 1

  def isZero(a: Int): Boolean = a == 0

  def notZero(a: Int): Boolean = a != 0
}
