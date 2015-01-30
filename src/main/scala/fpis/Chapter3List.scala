package fpis

/** Exercises for Chapter 3 of FPinS. */
sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }
  
  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs)   => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  
  // Exercise 3.2
  def tail[A](as: List[A]) = as match {
    case Nil        => Nil // Alternatively, could throw exception
    case Cons(_,xs) => xs
  }
  
  // Exercise 3.3
  def setHead[A](newHead: A, as: List[A]) = as match {
    case Nil        => Nil // There is no head to replace.
    case Cons(_,xs) => Cons(newHead,xs)
  }
  
  // Exercise 3.4
  def drop[A](as: List[A], n: Int): List[A] = {
    if (n <= 0) as // Hmmm. An error might be better.
    else as match {
      case Nil => Nil
      case Cons(x,xs) => drop(xs, n - 1)
    }
  }
  
  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _                 => l
  }
  
  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil         => Nil
    case Cons(_,Nil) => Nil
    case Cons(h,t)   => Cons(h,init(t))
  }
}