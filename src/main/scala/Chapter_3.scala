import annotation.tailrec

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
        case Nil => sys.error("tail on empty list")
        case Cons(_, xs) => xs
      }

      def setHead[A](as: List[A], a: A): List[A] = as match {
        case Nil => sys.error("setHead on empty list")
        case Cons(_, xs) => Cons(a, xs)
      }

      def drop[A](l: List[A], n: Int): List[A] =
        if (n <= 0) l
        else l match {
          case Nil => Nil
          case Cons(_, xs) => drop(xs, n - 1)
        }

      def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _ => l
      }

      def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = l match {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _ => l
      }

      def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }

      def init[A](l: List[A]): List[A] = l match {
        case Nil => sys.error("init in empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }

      def init2[A](l: List[A]): List[A] = {
        val buf = new collection.mutable.ListBuffer[A]
        @annotation.tailrec
        def go(cur: List[A]): List[A] = cur match {
          case Nil => sys.error("init in empty list")
          case Cons(_, Nil) => List(buf.toList: _*)
          case Cons(h, t) => buf += h; go(t)
        }
        go(l)
      }

      def init3[A](l: List[A]): List[A] = {
        reverse(tail(reverse(l)))
      }

      def reverse[A](l: List[A]): List[A] = {
        @annotation.tailrec
        def go(cur: List[A], acc: List[A]): List[A] = cur match {
          case Nil => sys.error("reverse in empty list")
          case Cons(h, Nil) => Cons(h, acc)
          case Cons(h, t) => go(t, Cons(h, acc))
        }

        go(l, Nil)
      }

      def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
          case Nil => z
          case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

      @tailrec
      def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
          case Nil => z
          case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }
      
      // when we do this, we simply get the list back itself
      // because foldRight implementation is basicaly the same thing as the
      // list constructor
      def exercise_3_8() = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

      def length1[A](l: List[A]): Int =
        foldRight(l, 0)((_,acc) => acc + 1)

      def length2[A](l: List[A]): Int =
        foldLeft(l, 0)((acc,_) => acc + 1)

      def sum2(l: List[Int]): Int =
        foldLeft(l, 0)(_ + _)

      def product2(l: List[Double]): Double =
        foldLeft(l, 1.0)(_ * _)

      def reverseFoldLeft[A](l: List[A]) =
        foldLeft(l, Nil:List[A])((acc, x) => Cons(x, acc))
      
      def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
        foldRight(reverse(as), z)((a: A, b: B) => f(b, a))
       
      def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(reverse(as), z)((b, a) => f(a, b))
      
      def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = 
        foldRight(a1, a2)(Cons(_,_))

      def flatten[A](l: List[List[A]]): List[A] =
        foldLeft(l, Nil:List[A])(append)
      
      def addOne(l: List[Int]): List[Int] =
        foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))
      
      def doubleToString(l: List[Double]): List[String] =
        foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

      def mapViaFoldRigh[A,B](as: List[A])(f: A => B): List[B] =
        foldRightViaFoldLeft(as, Nil:List[B])((h, t) => Cons(f(h), t))

      def map[A,B](l: List[A])(f: A => B): List[B] = {
        val buf = new scala.collection.mutable.ListBuffer[B]
        @tailrec
        def loop(l: List[A]): Unit = l match {
          case Nil => Nil
          case Cons(h, t) => buf += f(h); loop(t)
        }
        loop(l)
        List(buf.toList: _*)
      }

      def filterViaFoldRight[A](as: List[A])(f: A => Boolean): List[A] =
        foldRightViaFoldLeft(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

      def filter[A](l: List[A])(f: A => Boolean): List[A] = {
        val buf = new scala.collection.mutable.ListBuffer[A]
        def loop(l: List[A]): Unit = l match {
          case Nil => Nil
          case Cons(h, t) => if (f(h)) buf += h; loop(t)
        }
        loop(l)
        List(buf.toList: _*)
      }

    // val x = List(1,2,3,4,5) match {
    //   case Cons(x, Cons(2, Cons(4, _))) => x
    //   case Nil => 42
    //   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
    //   case Cons(h, t) => h + sum(t)
    //   case _ => 101
    // }

    // 3.7
    // No. It's not possible, since foldRight transverses all the list
    // evaluating the inner arguments (which includes de foldRight recursive calls)
    // and only when the stack is empty, it goes back evaluating the functions itself.
    }
  }
}