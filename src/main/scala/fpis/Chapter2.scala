package fpis

import scala.annotation.tailrec

/** Exercises for Chapter 2 of FPinS. */
class Chapter2 {

  // Exercise 2.1
  /** Returns the nth Fibonacci number, n is a 0-based index. */
  def fib(n: Int): Int = {
    /** Returns the nth Fibonacci number by recursively going
      * from n down to 0 to calculate the sum.*/
    @tailrec def fibRecurse(n: Int, previous: Int, current: Int): Int = {
      if (n == 0) previous
      else        fibRecurse(n - 1, current, current + previous)
    }
    fibRecurse(n, 0, 1)
  }


  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop[A](n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }


  // Exercise 2.3. Currying.
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // Exercise 2.4. Uncurrying.
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
  
  // Exercise 2.5. Function composition.
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
