

object Main extends App {
  println(Chapter_2.formatResult("double", 4, (n: Int) => n * 2))
}

object Transversables {
  def reduceRightTesting() {
    val stringList = List("Do", "Re", "Me", "Fa", "So", "La", "Te", "Do")
    println(stringList.reduceLeft(_ + _))
  }

  def addStringTesting() {
    val stringBuilder = new StringBuilder()
    val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    stringBuilder.append("I want all numbers 6-12: ")
    list.filter(it => it > 5 && it < 13).addString(stringBuilder, ",")
    println(stringBuilder.mkString)
  }
}