import org.scalatest._
import Chapter_3._
import Chapter_3.SingleLinkedList.List._

class Chapter_3Test extends FlatSpec with Matchers {
  "list" should "put the items in the exact order as they are passed" in {
    val list = SingleLinkedList.List(10, 20, 30, 40, 50)
    stringfy(list) should be ("10, 20, 30, 40, 50");
  }

  "tail" should "remove the first element from the list" in {
    val list = SingleLinkedList.List(10, 20, 30, 40, 50)
    tail(list) should be (SingleLinkedList.List(20, 30, 40, 50))
  }

  "tail" should "return an empty list if the list is already empty" in {
    an [Exception] should be thrownBy (tail(SingleLinkedList.Nil))
  }

  "tail" should "return an empty list when the list has only one element" in {
    tail(SingleLinkedList.List(10)) should be (SingleLinkedList.Nil)
  }

  "setHead" should "replace the first element in a list" in {
    setHead(SingleLinkedList.List(10, 20, 30), 0) should be (SingleLinkedList.List(0, 20, 30))
  }

  "setHead" should "throw and exception if the list is empty" in {
    an [Exception] should be thrownBy (setHead(SingleLinkedList.Nil, 0))
  }

  "drop" should "remove the first n elements from a list" in {
    drop(SingleLinkedList.List(10, 20, 30, 40, 50, 60, 70, 80), 4) should be (SingleLinkedList.List(50, 60, 70, 80))
  }

  "drop" should "return an empty list if the list was already empty" in {
    drop(SingleLinkedList.Nil, 2) should be (SingleLinkedList.Nil)
  }

  "drop" should "return an empty list if n is equal or greater than the list length" in {
    drop(SingleLinkedList.List(10, 20, 30), 3) should be (SingleLinkedList.Nil)
    drop(SingleLinkedList.List(10, 20), 3) should be (SingleLinkedList.Nil)
  }

  "dropWhile" should "drop items from the list as long as they match the the predicate" in {
    dropWhile(SingleLinkedList.List(10, 20, 30, 40, 50), (x: Int) => x < 30) should be (SingleLinkedList.List(30, 40, 50))
  }

  "dropWhile" should "drop items from the list as long as they match the the predicate (and maximize type inference for f's parameter)" in {
    dropWhileCurried(SingleLinkedList.List(10, 20, 30, 40, 50))(x => x < 30) should be (SingleLinkedList.List(30, 40, 50))
  }

  "append" should "append second list in the end of the first list" in {
    append(
      SingleLinkedList.List(10, 20, 30),
      SingleLinkedList.List(40, 50, 60)
    ) should be (SingleLinkedList.List(10, 20, 30, 40, 50, 60))
  }

  "init" should "return all but the last element from a list" in {
    init(SingleLinkedList.List(10, 20, 30, 40, 50)) should be (SingleLinkedList.List(10, 20, 30, 40))
  }

  "init" should "throw and exception if the list is empty" in {
    an [Exception] should be thrownBy (init(SingleLinkedList.Nil))
  }

  "init2" should "return all but the last element from a list" in {
    init2(SingleLinkedList.List(10, 20, 30, 40, 50)) should be (SingleLinkedList.List(10, 20, 30, 40))
  }

  "init3" should "return all but the last element from a list" in {
    init3(SingleLinkedList.List(10, 20, 30, 40, 50)) should be (SingleLinkedList.List(10, 20, 30, 40))
  }

  "reverse" should "reverse the list" in {
    reverse(SingleLinkedList.List(10, 20, 30, 40, 50)) should be (SingleLinkedList.List(50, 40, 30, 20, 10))
  }

  "foldRight" should "fold from the right" in {
    foldRight(SingleLinkedList.List("a", "b", "c", "d", "e"), "x")(_ + _) should be ("abcdex")
  }

  "foldRight" should "return the initial value if list is empty" in {
    foldRight(SingleLinkedList.Nil, "x")((x: String, y: String) => x + y) should be ("x")
  }

  "foldLeft" should "fold from the left" in {
    foldLeft(SingleLinkedList.List("a", "b", "c", "d", "e"), "x")(_ + _) should be ("xabcde")
  }

  "foldLeft" should "return the initial value if list is empty" in {
    foldLeft(SingleLinkedList.Nil, "x")((x: String, y: String) => x + y) should be ("x")
  }

  "length" should "compute the length of the list" in {
    length1(SingleLinkedList.List(1, 2, 3, 4, 5)) should be (5)
    // length(SingleLinkedList.Nil:List[Int]) should be (0)
    // val as = Cons(1, Cons(2, Nil))

  }
}