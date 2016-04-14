package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {

  behavior of "Exercise 3.1"

  it should "be 3, I believe" in {
    List.x shouldEqual 3
  }

  behavior of "Exercise 3.2"

  it should "remove the first item of a list with length 4" in {
    List.tail(List(1, 2, 3, 4)) shouldEqual List(2, 3, 4)
  }

  it should "remove the first item of a list with length 2" in {
    List.tail(List(1, 2)) shouldEqual List(2)
  }

  it should "remove the first item of a list with length 1" in {
    List.tail(List(1)) shouldEqual Nil
  }

  it should "not explode on a Nil list" in {
    List.tail(Nil) shouldEqual Nil
  }

  behavior of "Exercise 3.3"

  it should "set the head of a list with length 3" in {
    List.setHead(List(2, 3, 4), 1) shouldEqual List(1, 2, 3, 4)
  }

  it should "set the head of a list with length 1" in {
    List.setHead(List(2), 1) shouldEqual List(1, 2)
  }

  it should "set the head of a Nil list" in {
    List.setHead(Nil, 1) shouldEqual List(1)
  }

  behavior of "Exercise 3.4"

  it should "drop the first 3 elements of a list with length 4" in {
    List.drop(List(1, 2, 3, 4), 3) shouldEqual List(4)
  }

  it should "drop all elements when n > length" in {
    List.drop(List(1, 2, 3, 4), 5) shouldEqual Nil
  }

  it should "not explode on a Nil list" in {
    List.drop(Nil, 2) shouldEqual Nil
  }

  behavior of "Exercise 3.5"

  it should "drop the initial odd elements of a list" in {
    def isOdd(a: Int) = {
      (a % 2) == 1
    }

    List.dropWhile(List(1, 2, 3, 4), isOdd) shouldEqual List(2, 3, 4)
  }

  it should "drop all elements of a list when the predicate is always true" in {
    def yop(a: Int) = true
    List.dropWhile(List(1, 2, 3, 4), yop) shouldEqual Nil
  }

  it should "drop no elements of a list when the predicate is always false" in {
    def nop(a: Int) = false
    List.dropWhile(List(1, 2, 3, 4), nop) shouldEqual List(1, 2, 3, 4)
  }

  it should "not explode on a Nil list" in {
    def yop(a: Int) = true
    List.dropWhile(Nil, yop) shouldEqual Nil
  }

  behavior of "Exercise 3.6"

  it should "drop the last element of a list with length 4" in {
    List.init(List(1, 2, 3, 4)) shouldEqual List(1, 2, 3)
  }

  it should "drop a list with length 1" in {
    List.init(List(1)) shouldEqual Nil
  }

  it should "not explode on a Nil list" in {
    List.init(Nil) shouldEqual Nil
  }

  behavior of "Exercise 3.7"

  it should "short circuit at 0.0" in {
    List.foldRightShortCircuit(List(1, 2, 0, 4, 5), 1.0, 0, 0.0)(_ * _) shouldEqual 0.0
  }

  behavior of "Exercise 3.8"

  it should "enlighten me" in {
    // Fold right can perform the same behavior as the program stack, so it can make lists.  This is cool.
    List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_)) shouldEqual List(1, 2, 3)
  }

}
