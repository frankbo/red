package Chapters

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Chapter3 {
  def patternMatch(l: List[Int]): Int = l match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(_, xs) => xs
    case _ => l
  }

  def setHead[A](a: A, l: List[A]): List[A] = l match {
    case Cons(_, xs) => Cons(a, xs)
    case _ => l
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] =  l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def lengthR[A](as: List[A]): Int = foldRight(as, 0)((_, y) => y + 1)

  def sumR(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def productR(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumL(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def prodL(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def lengthL[A](ns: List[A]): Int = foldLeft(ns, 0)((x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((accList, nextVal) => Cons(nextVal, accList))

  def appendWithFold[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case _ => foldLeft(reverse(l1), l2)((t, next) => Cons(next, t))
  }
  def flatten[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())((accList, next) => List.append(accList, next))

  def addOneToEveryElement(l: List[Int]): List[Int] =
    map(l)((listElement) => listElement + 1)

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((next, t) => Cons(next.toString, t))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((x,y) => Cons(f(x), y))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((next, accList) =>
      if(f(next)) Cons(next, accList) else accList )

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((x: A) => if(f(x)) Cons(x, List()) else List())

  def zipWith[A](a: List[A], b: List[A], f: ((A, A)) => A): List[A] = {
    def calc(l1: List[A], l2: List[A], acc: List[(A, A)]): List[(A, A)] = (l1, l2) match {
      case (_, Nil) => acc
      case (Nil, _) => acc
      case (Cons(x, xs), Cons(y, ys)) => calc(xs, ys, List.append(acc, Cons((x, y), List())))
    }

    val tupleList = calc(a, b, List())
    map(tupleList)(f)
  }

  def treeSize[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => 1 + treeSize(l) + treeSize(r)
    case Leaf(_) => 1
  }

  def treeMax(tree: Tree[Int]): Int = tree match {
    case Branch(l, r) => treeMax(l) max treeMax(r)
    case Leaf(v) => v
  }

  def treeDepth[A](tree: Tree[A], depth: Int): Int = tree match {
    case Branch(l, r) => treeDepth(l, depth + 1) max treeDepth(r, depth + 1)
    case Leaf(_) => depth + 1
  }

  def treeMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(b1, b2) => Branch(treeMap(b1)(f), treeMap(b2)(f))
    case Leaf(a) => Leaf(f(a))
  }

  // Tree fold

  def treeFold[A, B](tree: Tree[A])(lf: A => B)(bf: (B, B) => B): B = tree match {
    case Branch(l, r) => bf(treeFold(l)(lf)(bf), treeFold(r)(lf)(bf))
    case Leaf(a) => lf(a)
  }

  def treeFoldCount[A](tree: Tree[A]): Int =
    treeFold(tree)((_) => 1)((b1, b2)=> 1 + b1 + b2)

  def treeFoldMax(tree: Tree[Int]): Int =
    treeFold(tree)(identity)((b1, b2)=> b1 max b2)

  def treeFoldDepth[A](tree: Tree[A]): Int =
    treeFold(tree)(_ => 1)((b1, b2)=> 1 + (b1 max b2))

  def treeFoldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    treeFold(tree)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r): Tree[B])

    // TODO 3.7, 3.8, 3.13, 3.24

}
