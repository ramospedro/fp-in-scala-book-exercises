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

  "variance" should "calculate the variance of the numbers" in {

    /*
    If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2)
    for each element x in the sequence.

    2 5 10 8 | mean = 6.25

    (2 - 6.25) ^ 2 = 18.0625 | (5 - 6.25) ^ 2 = 1.5625 | (10 - 6.25) ^ 2 = 14.0625 | (8 - 6.25) ^ 2 = 3.0625

    mean = 9.1875 | variance = 9.1875
     */

    variance(Seq(2, 5, 10, 8)) should be(Some(9.1875))
  }

  "variance" should "return None if the list is empty" in {
    variance(Nil: Seq[Double]) should be(None)
  }
}
