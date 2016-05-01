package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  "Option" should "compare options in" in {
    Some(5) should be(Some(5))
    Some("foo") should be(Some("foo"))
    None should be(None)

    Some(5) shouldNot be(Some(7))
    None shouldNot be(Some(5))
  }

  // 4.3
  it should "combine options when both are Some" in {
    Option.map2(Some(5), Some(7))(_ + _) should be(Some(12))
  }

  it should "combine options when one is None" in {
    Option.map2(Some(5), None)(_ + _) should be(None)
    Option.map2[Int, Int, Int](None, Some(5))(_ + _) should be(None)
  }

  // 4.4

  it should "sequence a list with all Somes" in {
    val in = List(Some(3), Some(4), Some(5))
    val expected = List(3,4,5)

    val out = Option.sequence(in)

    out shouldBe a [Some[_]]
    out.asInstanceOf[Some[Int]].get shouldBe expected
  }

  it should "sequence a list with a None" in {
    val in = List(Some(3), None, Some(5))
    val expected = List(3, 5)

    val out = Option.sequence(in)

    out shouldBe None
  }

  it should "sequencefold a list with all Somes" in {
    val in = List(Some(3), Some(4), Some(5))
    val expected = List(3,4,5)

    val out = Option.sequencefold(in)

    out shouldBe a [Some[_]]
    out.asInstanceOf[Some[Int]].get shouldBe expected
  }

  it should "sequencefold a list with a None" in {
    val in = List(Some(3), None, Some(5))
    val expected = List(3, 5)

    val out = Option.sequencefold(in)

    out shouldBe None
  }

  it should "traverse a list where f always returns a Some" in {
    val in = List(1, 2, 3, 4)
    val expected = List(2, 4, 6, 8)

    val out = Option.traverse(in)(x => Some(x * 2))

    out shouldBe a[Some[_]]
    out.getOrElse(List[Int]()) shouldBe expected
  }

  it should "traverse a list where f sometimes returns a None" in {
    val in = List(1, 2, 3, 4)

    val out = Option.traverse(in)(x => if (x == 3) None else Some(x))

    out shouldBe None
  }

  it should "sequencetraverse a list with all Somes" in {
    val in = List(Some(3), Some(4), Some(5))
    val expected = List(3,4,5)

    val out = Option.sequencetraverse(in)

    out shouldBe a [Some[_]]
    out.asInstanceOf[Some[Int]].get shouldBe expected
  }

  it should "sequencetraverse a list with a None" in {
    val in = List(Some(3), None, Some(5))
    val expected = List(3, 5)

    val out = Option.sequencetraverse(in)

    out shouldBe None
  }
}
