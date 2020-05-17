import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import Chapter_2.{Monomorphic, Polymorphic, Exercises}

class Chapter_2Test extends AnyFlatSpec with Matchers {
  "fibonacci first value" should "be 0" in {
    Exercises.fibonacci_2_1(1) should be(0)
  }

  "fibonacci second value" should "be 1" in {
    Exercises.fibonacci_2_1(2) should be(1)
  }

  "fibonacci values" should "be the sum of the last two" in {
    Exercises.fibonacci_2_1(3) should be(1)
    Exercises.fibonacci_2_1(4) should be(2)
    Exercises.fibonacci_2_1(5) should be(3)
    Exercises.fibonacci_2_1(6) should be(5)
    Exercises.fibonacci_2_1(7) should be(8)
  }

  "monomorphic findFirst" should "return the nth index of the value" in {
    Monomorphic
      .findFirst(Array("val", "var", "object"), "var") should be(1)
  }

  "monomorphic findFirst" should "inform -1 when value is not found" in {
    Monomorphic
      .findFirst(Array("val", "var", "object"), "class") should be(-1)
  }

  "polymorphic findFirst" should "return the nth index of the value" in {
    Polymorphic
      .findFirst[Int](Array(12, 20, 30, 40), (n: Int) => n == 40) should be(3)
  }

  "polymorphic findFirst" should "inform -1 when value is not found" in {
    Polymorphic
      .findFirst[Int](Array(12, 20, 30, 40), (n: Int) => n == 50) should be(-1)
  }

  "isSorted" should "inform when array is sorted according to the given function" in {
    def numerically(n: Int, x: Int): Boolean = n > x
    Exercises.isSorted_2_2[Int](Array(1, 2, 3, 4, 5), numerically) should be(
      true
    )
  }

  "isSorted" should "inform when array is not sorted according to the given function" in {
    def numerically(n: Int, x: Int): Boolean = n > x
    Exercises.isSorted_2_2[Int](Array(2, 1, 3, 4, 5), numerically) should be(
      false
    )
  }

  "partial1" should "partially apply the argument" in {
    val sum = (a: Int, b: Int) => a + b
    val sumToFive = Chapter_2.partial1(5, sum)
    sumToFive(3) should be(8)
  }

  "curry" should "return a function that partially applies the first argument" in {
    val sum = (a: Int, b: Int) => a + b
    val sumCurried = Exercises.curry_2_3(sum)
    val sumToFive = sumCurried(5)
    sumToFive(3) should be(8)
  }

  "uncurry" should "return a function that receives two arguments" in {
    val sumCurried = (a: Int) => (b: Int) => a + b
    val sumUncurried = Exercises.uncurry_2_4(sumCurried)
    sumUncurried(5, 3) should be(8)
  }

  "compose" should "feed the result of 'g' to 'f' – basically f(g(x))" in {
    val f = (x: Int) => x * 2
    val g = (x: Int) => x + 1
    Exercises.compose_2_5(f, g)(5) should be(12)
  }
}
