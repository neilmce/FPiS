package fpis

/** Exercises for Chapter 5 of FPinS. */
sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = {
    if (n > 0)
    this match {
      case Empty => Empty
      case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
    }
    else Empty
  }
  
  def drop(n: Int): Stream[A] = {
    if (n > 0)
      this match {
        case Empty => Empty
        case Cons(_, t) => t().drop(n - 1)
      }
    else this
  }
  
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Empty
    }
  }
  
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }
  
  def takeWhileViaFR(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Stream.empty)
  }
  
  def headOptionViaFR(): Option[A] = {
    foldRight(None: Option[A])((h,_) => Some(h))
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  
  // 'smart constructors'
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object StreamExercises {
  def ones: Stream[Int] = Stream.cons(1, ones)
  
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
  
  def fibs: Stream[Int] = {
    def next(n: Int, m: Int): Stream[Int] = Stream.cons(n, next(m, n + m))
    next(0, 1)
  }
  
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some((a,s)) => Stream.cons(a, unfold(s)(f))
    }
  }
  
  def fibsViaUnfold(): Stream[Int] = {
    unfold((0, 1)) { case (n,m) => Some((n, (m, n + m))) }
  }
}