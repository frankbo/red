package Chapters.Chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_)      => 1
  }

  def tMax(tree: Tree[Int]): Int = tree match {
    case Branch(l, r) => tMax(l) max tMax(r)
    case Leaf(v)      => v
  }

  def depth[A](tree: Tree[A], depthAcc: Int): Int = tree match {
    case Branch(l, r) => depth(l, depthAcc + 1) max depth(r, depthAcc + 1)
    case Leaf(_)      => depthAcc + 1
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(b1, b2) => Branch(map(b1)(f), map(b2)(f))
    case Leaf(a)        => Leaf(f(a))
  }

  def fold[A, B](tree: Tree[A])(lf: A => B)(bf: (B, B) => B): B = tree match {
    case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
    case Leaf(a)      => lf(a)
  }

  def foldCount[A](tree: Tree[A]): Int =
    fold(tree)((_) => 1)((b1, b2) => 1 + b1 + b2)

  def foldMax(tree: Tree[Int]): Int =
    fold(tree)(identity)((b1, b2) => b1 max b2)

  def foldDepth[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((b1, b2) => 1 + (b1 max b2))

  def foldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r): Tree[B])
}
