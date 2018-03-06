package FPChapters {

  object Chapter2 {

    def fib(n: Int): Int = {
      def go(nPrev: Int, nCurr: Int, n: Int): Int = {
        if (n == 0) nPrev
        else go(nCurr, nPrev + nCurr, n - 1)
      }
      go(0, 1, n)
    }

    def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = {
      def go(as: List[A]): Boolean = {
        as match {
          case Nil => true
          case a :: Nil => true
          case a :: list =>
            if(ordered(a, list.head)) go(list.tail)
            else false
        }
      }
      go(as)
    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      def go(n: Int): Boolean = {
        if (n >= as.length - 1) true
        else if (ordered(as(n), as(n + 1))) go(n + 1)
        else false
      }
      go(0)
    }

    def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
      (b: B) => f(a, b)

    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
      a => b => f(a, b)

    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
      (a, b) => f(a)(b)

    def compose[A, B, C](f: B => C, g: A => B): A => C =
      a => f(g(a))
  }

}