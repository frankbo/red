package Chapters.Chapter3

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {
  "size" should "return the number of nodes (leaves, branches)" in {
    val tree = Branch(Leaf("a"), Leaf("b"))
    Tree.size(tree) shouldEqual 3
  }

  "max" should "return the highest Int value of the leaves" in {
    val tree = Branch(Branch(Leaf(4), Leaf(22)), Leaf(7))
    Tree.tMax(tree) shouldEqual 22
  }

  "depth" should "return the path length from root" in {
    val tree = Branch(
      Leaf(1),
      Branch(
        Leaf(1),
        Branch(Leaf(1),
          Branch(Leaf(1), Leaf(1))))
    )
    Tree.depth(tree, 0) shouldEqual 5
  }

  "map" should "change every leave based on the passed in function" in {
    val tree = Branch(Branch(Leaf(4), Leaf(22)), Branch(Leaf(7), Leaf(21)))
    val expectedTree = Branch(Branch(Leaf(5), Leaf(23)), Branch(Leaf(8), Leaf(22)))
    Tree.map(tree)(addOne) shouldEqual expectedTree
  }

  "fold" should "count every element in the tree" in {
    val tree = Branch(Branch(Leaf(4), Leaf(22)), Leaf(7))
    Tree.fold(tree)((_) => 1)((b1: Int, b2: Int) => 1 + b1 + b2) shouldEqual 5
  }

  "foldCount" should "count every element in the tree" in {
    val tree = Branch(Branch(Leaf(4), Leaf(22)), Leaf(7))
    Tree.foldCount(tree) shouldEqual 5
  }

  "foldMax" should "count every element in the tree" in {
    val tree = Branch(Branch(Leaf(4), Leaf(22)), Leaf(7))
    Tree.foldMax(tree) shouldEqual 22
  }

  "foldDepth" should "be the max depth of a tree" in {
    val tree = Branch(Branch(Leaf(4), Leaf(22)), Leaf(7))
    Tree.foldDepth(tree) shouldEqual 3
  }

  "foldMap" should "be the max depth of a tree" in {
    val tree = Branch(Branch(Leaf(4), Leaf(22)), Branch(Leaf(7), Leaf(21)))
    val expectedTree = Branch(Branch(Leaf(5), Leaf(23)), Branch(Leaf(8), Leaf(22)))
    Tree.foldMap(tree)(addOne) shouldEqual expectedTree
  }

  def addOne(a: Int): Int = a + 1

}
