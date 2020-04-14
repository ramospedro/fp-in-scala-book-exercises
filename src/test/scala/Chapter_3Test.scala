import org.scalatest._
import Chapter_3._

class Chapter_3Test extends FlatSpec with Matchers {
  "list" should "put the items in the exact order as they are passed" in {
    val list = SingleLinkedList.List(10, 20, 30, 40, 50)
    val stringfiedList =SingleLinkedList.List.stringfy(list)
    
    stringfiedList should be ("10, 20, 30, 40, 50");
  }
}