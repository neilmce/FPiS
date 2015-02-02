package fpis

/** Exercises for Chapter 3 of FPinS. */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(t: Tree[Int]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
  
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
  
  def fold[A,B](t: Tree[A])(map: A => B)(reduce: (B,B) => B): B = t match {
    case Leaf(v) => map(v)
    case Branch(l, r) => reduce(fold(l)(map)(reduce), fold(r)(map)(reduce))
  }
  
  def sizeByFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)
}