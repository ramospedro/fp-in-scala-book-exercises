

object Main extends App {
  println(fizzBuzz(20))

  def fizzBuzz(limit: Int): List[String] = {
    def loop(curr: Int, res: List[String]): List[String] = {
      def v =
        if (curr % 15 == 0) "fizzbuzz"
        else if (curr % 3 == 0) "fizz"
        else if (curr % 5 == 0) "buzz"
        else curr.toString
      
      if (curr == limit) res :+ v
      else loop(curr + 1, res :+ v)
    }
    loop(1, List[String]())
  }
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