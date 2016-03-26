package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + 1 + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](tree: Tree[A])(z: A => B)(f: (B, B) => B): B = tree match {
    case Leaf(value) => z(value)
    case Branch(left, right) =>
      val lrecur = fold(left)(z)(f)
      val rrecur = fold(right)(z)(f)
      f(lrecur, rrecur)
  }

  def sizefold[A](tree: Tree[A]): Int =
    fold(tree)(x => 1)((l,r) => l + r + 1)

  def maximumfold(tree: Tree[Int]): Int =
    fold(tree)(x => x)((l, r) => l max r)

  def depthfold[A](tree: Tree[A]): Int =
    fold(tree)(x => 1)((l,r) => (l max r) + 1)

  def mapfold[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tree)(x => Leaf(f(x)))((l,r) => Branch(l, r))
}