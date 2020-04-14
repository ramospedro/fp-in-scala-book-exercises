object Chapter_3 {
  object SingleLinkedList {
    sealed trait List[+A]
    object Nil extends List[Nothing]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]

    object List {
      def stringfy[A](as: List[A]): String = as match {
        case Nil => ""
        case Cons(x,xs) => x.toString.concat(" ").concat(stringfy(xs)).trim()
      }

      def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }
  }
}