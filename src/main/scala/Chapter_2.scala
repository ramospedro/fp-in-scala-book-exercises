import annotation.tailrec

object Chapter_2 {
  object SingleLinkedList {
    sealed trait List[A+]
    object Nil extends List[Nothing]
    case class Cons[A+](head: A, tail: List[A]): List[A] extends List[A]

    object List {
      def apply[A+](as: A*) {
        if (as.isEmpty) Nil

        Cons(as.head, as.tail)
      }
    }
  }

  def abs(n: Int) : Int =
    if (n < 0) -n
    else n

  def formatAbs(x: Int) : String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int) : Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int) : Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    
    go(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  object Monomorphic {
    def findFirst(ss: Array[String], key: String): Int = {
      def loop(n: Int): Int = {
        if (n >= ss.length) -1
        else if (ss(n) == key) n
        else loop(n + 1)
      }

      loop(0)
    }
  }

  object Polymorphic {
    def findFirst[A](as: Array[A], p: A => Boolean): Int = {
      def loop(n: Int): Int = {
        if (n >= as.length) -1
        else if (p(as(n))) n
        else loop(n + 1)
      }

      loop(0)
    }
  }

  object Exercises {
    def fibonacci_2_1(n: Int) : Int = {
      def go(nth: Int, previous: Int, current: Int) : Int =
        if (nth == n) previous
        else go(nth + 1, current, previous + current)

      go(1, 0, 1);
    }

    def isSorted_2_2[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
      def loop(nth: Int): Boolean =
        if (nth >= as.length - 1) true
        else if (gt(as(nth), as(nth + 1))) false
        else loop(nth + 1)

      loop(0)
    }

    def curry_2_3[A,B,C](f: (A, B) => C): A => (B => C) =
      (a: A) => (b: B) => f(a, b)
    
    def uncurry_2_4[A,B,C](f: A => B => C): (A, B) => C =
      (a: A, b: B) => f(a)(b)

    def compose_2_5[A,B,C](f: B => C, g: A => B): A => C =
      x => f(g(x))
  }
}