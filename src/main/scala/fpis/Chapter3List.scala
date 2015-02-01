package fpis

import scala.annotation.tailrec

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
  
  
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }
  
  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }
  
  // Exercise 3.10
  @tailrec def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
  
  // Exercise 3.11: sum, product & list.length using foldLeft
  def sumLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def productLeft(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  
  def lengthViaFL[A](as: List[A]): Int = foldLeft(as, 0)((acc,h) => acc + 1)
  
  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, h) => Cons(h, acc))
  
  // Exercise 3.13
  def foldLeftViaFR[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, z)((a: A, b: B) => f(b, a)) // FIXME
  }
  
  // Exercise 3.14
  def appendViaFR[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)(Cons(_,_))
  }
  
  // Exercise 3.15
  def concat[A](lists: List[List[A]]): List[A] = {
    foldLeft(lists, Nil: List[A])((r: List[A], l: List[A]) => List.appendViaFR(r, l))
  }
  
  // Exercise 3.16 - 18
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil       => Nil
    case Cons(h,t) => Cons(f(h), map(t)(f))
  }
  
  // Exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil       => Nil
    case Cons(h,t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }
  
  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h,t) => concat(List(f(h), flatMap(t)(f)))
  }
  
  // Exercise 3.21
  def filterViaFM[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)((a: A) => if (f(a)) List(a) else Nil)
  }
  
  // Exercise 3.22 & 3.23
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }
  
  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    
    def hasStart[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (Nil, _) => false
      case (_, Nil) => false
      case (Cons(h1,t1),Cons(h2,t2)) => h1 == h2 && hasStart(t1, t2)
    }
    
    sup match {
      case Cons(h,t) if hasStart(sup, sub) => true
      case Cons(h,t) => hasSubsequence(t, sub)
    }
  }
}