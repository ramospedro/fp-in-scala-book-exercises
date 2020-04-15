object Chapter_3 {
  object SingleLinkedList {
    sealed trait List[+A]
    object Nil extends List[Nothing]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]

    object List {
      def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
        
      def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x,xs) => x + sum(xs)
      }

      def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
      }

      def stringfy[A](as: List[A]): String = as match {
        case Nil => ""
        case Cons(x, Nil) => x.toString
        case Cons(x, xs) => x.toString + ", " + stringfy(xs)
      }

      def tail[A](as: List[A]): List[A] = as match {
        case Nil => Nil
        case Cons(_, xs) => xs
      }

      def setHead[A](as: List[A], a: A): List[A] = as match {
        case Nil => Nil
        case Cons(_, xs) => Cons(a, xs)
      }


    }

    // val x = List(1,2,3,4,5) match {
    //   case Cons(x, Cons(2, Cons(4, _))) => x
    //   case Nil => 42
    //   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
    //   case Cons(h, t) => h + sum(t)
    //   case _ => 101
    // }
    // result is 3, matching the third expression
  }
}