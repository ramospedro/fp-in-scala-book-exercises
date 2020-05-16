import org.scalatest._
import Chapter_3._
import Chapter_3.SLL.List._
import Chapter_3.BinaryTree._

class Chapter_3Test extends FlatSpec with Matchers {
  "list" should "put the items in the exact order as they are passed" in {
    val list = SLL.List(10, 20, 30, 40, 50)
    stringfy(list) should be("10, 20, 30, 40, 50");
  }

  "tail" should "remove the first element from the list" in {
    val list = SLL.List(10, 20, 30, 40, 50)
    tail(list) should be(SLL.List(20, 30, 40, 50))
  }

  "tail" should "return an empty list if the list is already empty" in {
    an[Exception] should be thrownBy (tail(SLL.Nil))
  }

  "tail" should "return an empty list when the list has only one element" in {
    tail(SLL.List(10)) should be(SLL.Nil)
  }

  "setHead" should "replace the first element in a list" in {
    setHead(SLL.List(10, 20, 30), 0) should be(SLL.List(0, 20, 30))
  }

  "setHead" should "throw and exception if the list is empty" in {
    an[Exception] should be thrownBy (setHead(SLL.Nil, 0))
  }

  "drop" should "remove the first n elements from a list" in {
    drop(SLL.List(10, 20, 30, 40, 50, 60, 70, 80), 4) should be(
      SLL.List(50, 60, 70, 80)
    )
  }

  "drop" should "return an empty list if the list was already empty" in {
    drop(SLL.Nil, 2) should be(SLL.Nil)
  }

  "drop" should "return an empty list if n is equal or greater than the list length" in {
    drop(SLL.List(10, 20, 30), 3) should be(SLL.Nil)
    drop(SLL.List(10, 20), 3) should be(SLL.Nil)
  }

  "dropWhile" should "drop items from the list as long as they match the the predicate" in {
    dropWhile(SLL.List(10, 20, 30, 40, 50), (x: Int) => x < 30) should be(
      SLL.List(30, 40, 50)
    )
  }

  "dropWhile" should "drop items from the list as long as they match the the predicate (and maximize type inference for f's parameter)" in {
    dropWhileCurried(SLL.List(10, 20, 30, 40, 50))(x => x < 30) should be(
      SLL.List(30, 40, 50)
    )
  }

  "append" should "append second list in the end of the first list" in {
    append(
      SLL.List(10, 20, 30),
      SLL.List(40, 50, 60)
    ) should be(SLL.List(10, 20, 30, 40, 50, 60))
  }

  "init" should "return all but the last element from a list" in {
    init(SLL.List(10, 20, 30, 40, 50)) should be(SLL.List(10, 20, 30, 40))
  }

  "init" should "throw and exception if the list is empty" in {
    an[Exception] should be thrownBy (init(SLL.Nil))
  }

  "init2" should "return all but the last element from a list" in {
    init2(SLL.List(10, 20, 30, 40, 50)) should be(SLL.List(10, 20, 30, 40))
  }

  "init3" should "return all but the last element from a list" in {
    init3(SLL.List(10, 20, 30, 40, 50)) should be(SLL.List(10, 20, 30, 40))
  }

  "reverse" should "reverse the list" in {
    reverse(SLL.List(10, 20, 30, 40, 50)) should be(
      SLL.List(50, 40, 30, 20, 10)
    )
  }

  "foldRight" should "fold from the right" in {
    foldRight(SLL.List("a", "b", "c", "d", "e"), "x")(_ + _) should be("abcdex")
  }

  "foldRight" should "return the initial value if list is empty" in {
    foldRight(SLL.Nil, "x")((x: String, y: String) => x + y) should be("x")
  }

  "foldLeft" should "fold from the left" in {
    foldLeft(SLL.List("a", "b", "c", "d", "e"), "x")(_ + _) should be("xabcde")
  }

  "foldLeft" should "return the initial value if list is empty" in {
    foldLeft(SLL.Nil, "x")((x: String, y: String) => x + y) should be("x")
  }

  "length1" should "compute the length of the list" in {
    length1(SLL.List(1, 2, 3, 4, 5)) should be(5)
  }

  "length2" should "compute the length of the list using foldLeft" in {
    length2(SLL.List(1, 2, 3, 4, 5)) should be(5)
  }

  "sum2" should "compute the sum of a list using foldLeft" in {
    sum2(SLL.List(10, 10, 20, 20)) should be(60)
  }

  "product2" should "compute the product of a list using foldLeft" in {
    product2(SLL.List(1.0, 2.0, 2.0, 3.0, 4.0)) should be(48.0)
  }

  "reverseFoldLeft" should "reverse the list using foldLeft" in {
    reverseFoldLeft(SLL.List(10, 20, 30, 40, 50)) should be(
      SLL.List(50, 40, 30, 20, 10)
    )
  }

  "foldLeftViaFoldRight" should "fold left the list via foldRight" in {
    foldLeftViaFoldRight(SLL.List("a", "b", "c", "d", "e"), "x")(_ + _) should be(
      "xabcde"
    )
  }

  "foldRightViaFoldLeft" should "fold right the list via foldLeft" in {
    foldRightViaFoldLeft(SLL.List("a", "b", "c", "d", "e"), "x")(_ + _) should be(
      "abcdex"
    )
  }

  "appendFoldRight" should "append second list in the end of the first list using foldRight" in {
    appendFoldRight(
      SLL.List(10, 20, 30),
      SLL.List(40, 50, 60)
    ) should be(SLL.List(10, 20, 30, 40, 50, 60))
  }

  "concat" should "convert a list of lists into a single list" in {
    concat(
      SLL.List(
        SLL.List(1, 2, 3, 4),
        SLL.List(5, 6, 7, 8),
        SLL.List(9, 10, 11, 12)
      )
    ) should be(SLL.List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  }

  "addOne" should "add 1 to each element in a list of integers" in {
    addOne(SLL.List(1, 2, 3)) should be(SLL.List(2, 3, 4))
  }

  "doubleToString" should "convert a list of double to a list of string" in {
    doubleToString(SLL.List(1.0, 2.0, 3.0, 4.1, 5.2)) should be(
      SLL.List("1.0", "2.0", "3.0", "4.1", "5.2")
    )
  }

  "mapViaFoldRigh" should "modify each element by applying the given function" in {
    mapViaFoldRigh(SLL.List(1, 2, 3, 4, 5))(_ * 2) should be(
      SLL.List(2, 4, 6, 8, 10)
    )
  }

  "map" should "modify each element by applying the given function while being stack safe" in {
    map(SLL.List(1, 2, 3, 4, 5))(_ * 2) should be(SLL.List(2, 4, 6, 8, 10))
  }

  "filter" should "remove all elements from a list that dont satisfy the predicate" in {
    filterViaFoldRight(SLL.List(1, 2, 3, 4, 5))(_ % 2 == 0) should be(
      SLL.List(2, 4)
    )
    filter(SLL.List(1, 2, 3, 4, 5))(_ % 2 == 0) should be(SLL.List(2, 4))
    filterViaFlatMap(SLL.List(1, 2, 3, 4, 5))(_ % 2 == 0) should be(
      SLL.List(2, 4)
    )
  }

  "flatMap" should "map the list applying the given function that returns a list instead of a single element" in {
    flatMapViaFoldRight(SLL.List(1, 2, 3))(i => SLL.List(i, i)) should be(
      SLL.List(1, 1, 2, 2, 3, 3)
    )
    flatMap(SLL.List(1, 2, 3))(i => SLL.List(i, i)) should be(
      SLL.List(1, 1, 2, 2, 3, 3)
    )
    flatMapViaConcat(SLL.List(1, 2, 3))(i => SLL.List(i, i)) should be(
      SLL.List(1, 1, 2, 2, 3, 3)
    )
  }

  "addPairwise" should "zip two lists adding the corresponding elements" in {
    addPairwise(SLL.List(1, 2, 3), SLL.List(4, 5, 6)) should be(
      SLL.List(5, 7, 9)
    )
    addPairwise(SLL.List(1, 2, 3, 4), SLL.List(4, 5, 6)) should be(
      SLL.List(5, 7, 9)
    )
    addPairwise(SLL.List(1, 2, 3), SLL.List(4, 5, 6, 4)) should be(
      SLL.List(5, 7, 9)
    )
  }

  "zipWith" should "zip two lists applying the function to the corresponding elements" in {
    zipWith(SLL.List(1, 2, 3), SLL.List(4, 5, 6))((x, y) => x + y) should be(
      SLL.List(5, 7, 9)
    )
    zipWith(SLL.List(1, 2, 3, 4), SLL.List(4, 5, 6))((x, y) => x + y) should be(
      SLL.List(5, 7, 9)
    )
    zipWith(SLL.List(1, 2, 3), SLL.List(4, 5, 6, 4))((x, y) => x + y) should be(
      SLL.List(5, 7, 9)
    )

    zipWithTailRecursive(SLL.List(1, 2, 3), SLL.List(4, 5, 6))((x, y) => x + y) should be(
      SLL.List(5, 7, 9)
    )
    zipWithTailRecursive(SLL.List(1, 2, 3, 4), SLL.List(4, 5, 6))((x, y) =>
      x + y
    ) should be(SLL.List(5, 7, 9))
    zipWithTailRecursive(SLL.List(1, 2, 3), SLL.List(4, 5, 6, 4))((x, y) =>
      x + y
    ) should be(SLL.List(5, 7, 9))
  }

  "startsWith" should "inform wether a list starts with another" in {
    startsWith(SLL.List(1, 2, 3, 4, 5), SLL.List(1, 2, 3)) should be(true)
    startsWith(SLL.List(1, 2, 3, 4, 5), SLL.List(2, 3)) should be(false)
    startsWith(SLL.Nil, SLL.Nil) should be(true)
  }

  "hasSubsequence" should "return true when the subsequence exists inside the supersequence" in {
    hasSubsequence(SLL.List(1, 2, 3, 4, 5), SLL.List(2, 2, 4)) should be(false)
  }

  "hasSubsequence" should "return false when the subsequence doesn't exist inside the supersequence" in {
    hasSubsequence(SLL.List(1, 2, 3, 4, 5), SLL.List(2, 3, 4)) should be(true)
  }

  "hasSubsequence" should "return true when the subsequence is empty for non-empty supersequences" in {
    hasSubsequence(SLL.List(1, 2), SLL.Nil) should be(true)
  }

  "hasSubsequence" should "return true when the subsequence is Nil for empty supersequences" in {
    hasSubsequence(SLL.Nil, SLL.Nil) should be(true)
  }

  "size" should "count the number of nodes in a tree" in {
    val treeSize3 = Branch(Leaf(1), Leaf(1))
    val treeSize5 = Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))
    val treeSize7 = Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1))
    val treeSize9 = Branch(
      Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1)),
      Leaf(1)
    )

    BinaryTree.size(treeSize3) should be(3)
    BinaryTree.size(treeSize5) should be(5)
    BinaryTree.size(treeSize7) should be(7)
    BinaryTree.size(treeSize9) should be(9)

    BinaryTree.sizeViaFold(treeSize3) should be(3)
    BinaryTree.sizeViaFold(treeSize5) should be(5)
    BinaryTree.sizeViaFold(treeSize7) should be(7)
    BinaryTree.sizeViaFold(treeSize9) should be(9)
  }

  "maximum" should "return the maximum element of a tree" in {
    val treeMaximum10 = Branch(Leaf(8), Leaf(10))
    val treeMaximum20 = Branch(Branch(Leaf(20), Leaf(19)), Leaf(5))
    val treeMaximum30 =
      Branch(Branch(Branch(Leaf(2), Leaf(29)), Leaf(30)), Leaf(4))
    val treeMaximum40 = Branch(
      Branch(Branch(Branch(Leaf(2), Leaf(40)), Leaf(38)), Leaf(0)),
      Leaf(1)
    )

    BinaryTree.maximum(treeMaximum10) should be(10)
    BinaryTree.maximum(treeMaximum20) should be(20)
    BinaryTree.maximum(treeMaximum30) should be(30)
    BinaryTree.maximum(treeMaximum40) should be(40)

    BinaryTree.maximumViaFold(treeMaximum10) should be(10)
    BinaryTree.maximumViaFold(treeMaximum20) should be(20)
    BinaryTree.maximumViaFold(treeMaximum30) should be(30)
    BinaryTree.maximumViaFold(treeMaximum40) should be(40)
  }

  "depth" should "return the maximum path length from the root of a tree to any leaf" in {
    val treeDepth1 = Branch(Leaf(8), Leaf(10))
    val treeDepth2 = Branch(Branch(Leaf(20), Leaf(19)), Leaf(5))
    val treeDepth3 =
      Branch(Branch(Branch(Leaf(2), Leaf(29)), Leaf(30)), Leaf(4))
    val treeDepth4 = Branch(
      Leaf(1),
      Branch(Leaf(2), Branch(Branch(Leaf(2), Leaf(40)), Leaf(38)))
    )

    BinaryTree.depth(treeDepth1) should be(1)
    BinaryTree.depth(treeDepth2) should be(2)
    BinaryTree.depth(treeDepth3) should be(3)
    BinaryTree.depth(treeDepth4) should be(4)

    BinaryTree.depthViaFold(treeDepth1) should be(1)
    BinaryTree.depthViaFold(treeDepth2) should be(2)
    BinaryTree.depthViaFold(treeDepth3) should be(3)
    BinaryTree.depthViaFold(treeDepth4) should be(4)
  }

  "map" should "modify each element of the tree with the given function" in {
    val tree = Branch(
      Leaf(1),
      Branch(Leaf(2), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))
    )
    val treeDoubled = Branch(
      Leaf(2),
      Branch(Leaf(4), Branch(Branch(Leaf(6), Leaf(8)), Leaf(10)))
    )

    BinaryTree.mapTree(tree)(_ * 2) should be(treeDoubled)
    BinaryTree.mapViaFold(tree)(_ * 2) should be(treeDoubled)
  }

  "fold" should "generically fold the tree and applying the function" in {
    val treeSize9 = Branch(
      Leaf(1),
      Branch(Leaf(2), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))
    )

    BinaryTree.fold(treeSize9)(n => 1)((l, r) => 1 + l + r) should be(
      9
    )
  }
}
