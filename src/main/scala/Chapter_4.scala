//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

object Chapter_4 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None    => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None    => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this map (f) getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] =
      this flatMap (a => if (f(a)) Some(a) else None)
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (
        m => mean(xs.map(x => math.pow(x - m, 2)))
    )

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  // business function
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    age + numberOfSpeedingTickets;

  // service function called by some sort of controller or handler
  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

}
