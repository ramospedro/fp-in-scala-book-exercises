import org.scalatest._
import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec

import Chapter_4._

class Chapter_4Test extends AnyFlatSpec with Matchers {
  "map" should "apply f to the value if it's Some" in {
    val someValue: Option[Int] = Some(1)
    someValue.map(a => a * 2) should be(Some(2))

    val noneValue: Option[Int] = None
    noneValue.map(a => a * 2) should be(None)
  }

  "flatMap" should "apply f to the value if it's Some" in {
    val someValue: Option[Int] = Some(1)
    someValue.flatMap(a => Some(a * 2)) should be(Some(2))

    val noneValue: Option[Int] = None
    noneValue.flatMap(a => Some(a * 2)) should be(None)
  }

  "getOrElse" should "return the value or a default" in {
    val someValue: Option[Int] = Some(1)
    someValue.getOrElse(5) should be(1)

    val noneValue: Option[Int] = None
    noneValue.getOrElse(5) should be(5)
  }

  "orElse" should "return the option it's some or else the given alternative option" in {
    val someValue: Option[Int] = Some(1)
    someValue.orElse(Some(5)) should be(Some(1))

    val noneValue: Option[Int] = None
    noneValue.orElse(Some(5)) should be(Some(5))
  }

  "filter" should "return None if the predicate it not satisfied" in {
    val someValue: Option[Int] = Some(1)
    someValue.filter(a => a % 2 == 0) should be(None)
    someValue.filter(a => a % 2 == 1) should be(Some(1))

    val noneValue: Option[Int] = None
    noneValue.filter(_ => true) should be(None)
  }
}
