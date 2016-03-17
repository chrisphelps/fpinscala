package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(head, tail) => tail
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(head, tail) => Cons(h, tail)
    case Nil => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else l
    case Nil => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(head, Cons(last, Nil)) => Cons(head, Nil)
    case Cons(head, tail) => Cons(head, init(tail))
    case Nil => Nil
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a: A, b: Int) => b + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    case Nil => z
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(head, tail) => Cons(f(head), map(tail)(f))
    case Nil => Nil
  }
}

object Functions {

  def sum(l: List[Int]) = List.foldLeft(l, 0)(_ + _)
  def product(l: List[Int]) = List.foldLeft(l, 1)(_ * _)
  def lengthFL(l: List[Int]) = List.foldLeft(l, 0)((b: Int, a: Int) => b + 1)

  def reverse[A](l: List[A]) = List.foldLeft(l, Nil:List[A])((b: List[A], a: A) => Cons(a, b))

  // 3.13

  // hard and optional. thought about it but referred to the answer instead of solving. maybe try later

  // 3.14

  def append[A](a1: List[A], a2: List[A]): List[A] = List.foldRight(a1, a2)((a: A, b: List[A]) => Cons(a, b))


  // 3.15

  def concat[A](l: List[List[A]]): List[A] = {
    List.foldRight(l, Nil:List[A])(append[A])
  }


  // 3.16

  def addOne(l: List[Int]): List[Int] = l match {
    case Cons(head, tail) => Cons(head + 1, addOne(tail))
    case Nil => Nil
  }

  // 3.17

  def stringify(l: List[Double]): List[String] = l match {
    case Cons(head, tail) => Cons(head.toString, stringify(tail))
    case Nil => Nil
  }

  // 3.19

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if f(head) => Cons(head, filter(tail)(f))
    case Cons(head, tail) => filter(tail)(f)
    case Nil => Nil
  }

  def removeOdd(l: List[Int]): List[Int] = filter(l)(x => x % 2 == 1)

  // 3.20

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(List.map(l)(f))

  // 3.21

  def filterFM[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else List())

  // 3.22

  def addPairs(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(head1 + head2, addPairs(tail1, tail2))
    case (list, Nil) => list
    case (Nil, list) => list
  }

  // 3.23

  // if we stop once they are paired we can make this (List[A],List[B)((A,B)=>C) => List[C]
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(f(head1,head2), zipWith(tail1, tail2)(f))
    case (list, Nil) => list
    case (Nil, list) => list
  }

  // 3.24

  // hasSubsequence(non-nil, nil) == true
  // hasSubsequence(nil, non-nil) == false
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(head1, tail1), Cons(head2, tail2)) if (head1 == head2) => hasSubsequence(tail1, tail2)
    case (Cons(head1, tail1), Cons(head2, tail2)) => hasSubsequence(tail1, sub)
    case (Cons(head, tail), Nil) => true
    case (Nil, Cons(head, tail)) => false
    case (Nil, Nil) => true
  }
}