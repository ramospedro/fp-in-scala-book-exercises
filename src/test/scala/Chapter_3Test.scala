import org.scalatest._
import Chapter_3._
import Chapter_3.SinglyLinkedList.List._

class Chapter_3Test extends FlatSpec with Matchers {
  "list" should "put the items in the exact order as they are passed" in {
    val list = SinglyLinkedList.List(10, 20, 30, 40, 50)
    stringfy(list) should be ("10, 20, 30, 40, 50");
  }

  "tail" should "remove the first element from the list" in {
    val list = SinglyLinkedList.List(10, 20, 30, 40, 50)
    tail(list) should be (SinglyLinkedList.List(20, 30, 40, 50))
  }

  "tail" should "return an empty list if the list is already empty" in {
    an [Exception] should be thrownBy (tail(SinglyLinkedList.Nil))
  }

  "tail" should "return an empty list when the list has only one element" in {
    tail(SinglyLinkedList.List(10)) should be (SinglyLinkedList.Nil)
  }

  "setHead" should "replace the first element in a list" in {
    setHead(SinglyLinkedList.List(10, 20, 30), 0) should be (SinglyLinkedList.List(0, 20, 30))
  }

  "setHead" should "throw and exception if the list is empty" in {
    an [Exception] should be thrownBy (setHead(SinglyLinkedList.Nil, 0))
  }

  "drop" should "remove the first n elements from a list" in {
    drop(SinglyLinkedList.List(10, 20, 30, 40, 50, 60, 70, 80), 4) should be (SinglyLinkedList.List(50, 60, 70, 80))
  }

  "drop" should "return an empty list if the list was already empty" in {
    drop(SinglyLinkedList.Nil, 2) should be (SinglyLinkedList.Nil)
  }

  "drop" should "return an empty list if n is equal or greater than the list length" in {
    drop(SinglyLinkedList.List(10, 20, 30), 3) should be (SinglyLinkedList.Nil)
    drop(SinglyLinkedList.List(10, 20), 3) should be (SinglyLinkedList.Nil)
  }

  "dropWhile" should "drop items from the list as long as they match the the predicate" in {
    dropWhile(SinglyLinkedList.List(10, 20, 30, 40, 50), (x: Int) => x < 30) should be (SinglyLinkedList.List(30, 40, 50))
  }

  "dropWhile" should "drop items from the list as long as they match the the predicate (and maximize type inference for f's parameter)" in {
    dropWhileCurried(SinglyLinkedList.List(10, 20, 30, 40, 50))(x => x < 30) should be (SinglyLinkedList.List(30, 40, 50))
  }

  "append" should "append second list in the end of the first list" in {
    append(
      SinglyLinkedList.List(10, 20, 30),
      SinglyLinkedList.List(40, 50, 60)
    ) should be (SinglyLinkedList.List(10, 20, 30, 40, 50, 60))
  }

  "init" should "return all but the last element from a list" in {
    init(SinglyLinkedList.List(10, 20, 30, 40, 50)) should be (SinglyLinkedList.List(10, 20, 30, 40))
  }

  "init" should "throw and exception if the list is empty" in {
    an [Exception] should be thrownBy (init(SinglyLinkedList.Nil))
  }

  "init2" should "return all but the last element from a list" in {
    init2(SinglyLinkedList.List(10, 20, 30, 40, 50)) should be (SinglyLinkedList.List(10, 20, 30, 40))
  }

  "init3" should "return all but the last element from a list" in {
    init3(SinglyLinkedList.List(10, 20, 30, 40, 50)) should be (SinglyLinkedList.List(10, 20, 30, 40))
  }

  "reverse" should "reverse the list" in {
    reverse(SinglyLinkedList.List(10, 20, 30, 40, 50)) should be (SinglyLinkedList.List(50, 40, 30, 20, 10))
  }

  "foldRight" should "fold from the right" in {
    foldRight(SinglyLinkedList.List("a", "b", "c", "d", "e"), "x")(_ + _) should be ("abcdex")
  }

  "foldRight" should "return the initial value if list is empty" in {
    foldRight(SinglyLinkedList.Nil, "x")((x: String, y: String) => x + y) should be ("x")
  }

  "foldLeft" should "fold from the left" in {
    foldLeft(SinglyLinkedList.List("a", "b", "c", "d", "e"), "x")(_ + _) should be ("xabcde")
  }

  "foldLeft" should "return the initial value if list is empty" in {
    foldLeft(SinglyLinkedList.Nil, "x")((x: String, y: String) => x + y) should be ("x")
  }

  "length1" should "compute the length of the list" in {
    length1(SinglyLinkedList.List(1, 2, 3, 4, 5)) should be (5)
  }

  "length2" should "compute the length of the list using foldLeft" in {
    length2(SinglyLinkedList.List(1, 2, 3, 4, 5)) should be (5)
  }

  "sum2" should "compute the sum of a list using foldLeft" in {
    sum2(SinglyLinkedList.List(10, 10, 20, 20)) should be (60)
  }

  "product2" should "compute the product of a list using foldLeft" in {
    product2(SinglyLinkedList.List(1.0, 2.0, 2.0, 3.0, 4.0)) should be (48.0)
  }

  "reverseFoldLeft" should "reverse the list using foldLeft" in {
    reverseFoldLeft(SinglyLinkedList.List(10, 20, 30, 40, 50)) should be (SinglyLinkedList.List(50, 40, 30, 20, 10))
  }

  "foldLeftViaFoldRight" should "fold left the list via foldRight" in {
    foldLeftViaFoldRight(SinglyLinkedList.List("a", "b", "c", "d", "e"), "x")(_ + _) should be ("xabcde")
  }

  "foldRightViaFoldLeft" should "fold right the list via foldLeft" in {
    foldRightViaFoldLeft(SinglyLinkedList.List("a", "b", "c", "d", "e"), "x")(_ + _) should be ("abcdex")
  }

  "appendFoldRight" should "append second list in the end of the first list using foldRight" in {
    appendFoldRight(
      SinglyLinkedList.List(10, 20, 30),
      SinglyLinkedList.List(40, 50, 60)
    ) should be (SinglyLinkedList.List(10, 20, 30, 40, 50, 60))
  }

  "flatten" should "convert a list of lists into a single list" in {
    flatten(SinglyLinkedList.List(
      SinglyLinkedList.List(1, 2, 3, 4),
      SinglyLinkedList.List(5, 6, 7, 8),
      SinglyLinkedList.List(9, 10, 11, 12)
    )) should be (SinglyLinkedList.List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  }

  "addOne" should "add 1 to each element in a list of integers" in {
    addOne(SinglyLinkedList.List(1, 2, 3)) should be (SinglyLinkedList.List(2, 3, 4))
  }

  "doubleToString" should "convert a list of double to a list of string" in {
    doubleToString(
      SinglyLinkedList.List(1.0, 2.0, 3.0, 4.1, 5.2)) should be (SinglyLinkedList.List("1.0", "2.0", "3.0", "4.1", "5.2"))
  }

  "mapViaFoldRigh" should "modify each element by applying the given function" in {
    mapViaFoldRigh(SinglyLinkedList.List(1, 2, 3, 4, 5))( _ * 2) should be (SinglyLinkedList.List(2, 4, 6, 8, 10))
  }

  "map" should "modify each element by applying the given function while being stack safe" in {
    map(SinglyLinkedList.List(1, 2, 3, 4, 5))(_ * 2) should be (SinglyLinkedList.List(2, 4, 6, 8, 10))
  }

  "filter" should "remove all elements from a list that dont satisfy the predicate" in {
    filterViaFoldRight(SinglyLinkedList.List(1, 2, 3, 4, 5))(_ % 2 == 0) should be (SinglyLinkedList.List(2, 4))
    filter(SinglyLinkedList.List(1, 2, 3, 4, 5))(_ % 2 == 0) should be (SinglyLinkedList.List(2, 4))
  }
}