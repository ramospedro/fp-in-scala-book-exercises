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
    tail(SingleLinkedList.Nil) should be (SingleLinkedList.Nil)
  }

  "tail" should "return an empty list when the list has only one element" in {
    tail(SingleLinkedList.List(10)) should be (SingleLinkedList.Nil)
  }

  "setHead" should "replace the first element in a list" in {
    setHead(SingleLinkedList.List(10, 20, 30), 0) should be (SingleLinkedList.List(0, 20, 30))
  }

  "setHead" should "return an empty list if the list is already empty" in {
    setHead(SingleLinkedList.Nil, 0) should be (SingleLinkedList.Nil)
  }
}