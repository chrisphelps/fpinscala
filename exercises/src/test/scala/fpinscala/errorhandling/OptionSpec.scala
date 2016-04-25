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
}
