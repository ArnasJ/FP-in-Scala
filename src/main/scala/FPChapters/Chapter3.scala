package FPChapters

object Chapter3 {

  sealed trait CustomList[+A]
  case object Nil extends CustomList[Nothing]
  case class Cons[+A](head: A, tail: CustomList[A]) extends CustomList[A]

  object CustomList {
    def sum(ints: CustomList[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: CustomList[Double]): Double = ds match {
      case Nil => 1
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): CustomList[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](xs: CustomList[A]): CustomList[A] = xs match {
    case Nil => sys.error("tail on empty list")
    case Cons(_, t) => t
  }

  def setHead[A](xs: CustomList[A], newHead: A): CustomList[A] = xs match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(newHead, t)
  }

  def drop[A](l: CustomList[A], n: Int): CustomList[A] =
    if (n <= 0) l
    else l match {
    case Nil => Nil
    case Cons(_, t) => drop(t, n - 1)
  }

  def dropWhile[A](l: CustomList[A], f: A => Boolean): CustomList[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def dropWhile2[A](l: CustomList[A])(f: A => Boolean): CustomList[A] = l match {
    case Cons(h, t) if f(h) => dropWhile2(t)(f)
    case _ => l
  }

  def init[A](l: CustomList[A]): CustomList[A] = l match {
    case Nil => sys.error("init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: CustomList[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: CustomList[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: CustomList[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: CustomList[A]): Int =
    foldRight(as, 0)((_, i) => i + 1)

  def foldLeft[A, B](as: CustomList[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sum3(ns: CustomList[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: CustomList[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: CustomList[A]): Int =
    foldLeft(as, 0)((i, _) => i + 1)

  def _reverse[A](as: CustomList[A]): CustomList[A] =
    foldLeft(as, Nil: CustomList[A])((xs, a) => Cons(a, xs))

  def foldRightViaFoldLeft[A, B](as: CustomList[A], z: B)(f: (A, B) => B): B =
    foldLeft(as.reverse, z)((b, a) => f(a, b))

  def append[A](left: CustomList[A], right: CustomList[A]): CustomList[A] =
    foldRight(left, right)(Cons(_, _))

  def concat[A](as: CustomList[CustomList[A]]): CustomList[A] =
    foldRight(as, CustomList[A]())(append)

    implicit class ListExtensions[A](as: CustomList[A]){
      def length: Int = length2(as)
      def reverse: CustomList[A] = _reverse(as)
    }


}
