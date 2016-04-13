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
}
