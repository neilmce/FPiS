package fpis

/** Exercises for Chapter 4 of FPinS. */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  
  def orElse[B >: A](ob: Option[B]): Option[B] = {
    map(a => Some(a)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if(f(a)) Some(a) else None)
  }
}

case class  Some[+A](get: A) extends Option[A]
case object None             extends Option[Nothing]


object OptionExercises {
  def variance(xs: Seq[Double]): Option[Double] = {

    def mean(nums: Seq[Double]): Option[Double] = if (nums.isEmpty) None else Some(nums.foldRight(0.0)(_ + _) / nums.size)

    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aval => b.map(bval => f(aval, bval)))
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try {
      age.toInt
    }
    val optTickets: Option[Int] = Try {
      numberOfSpeedingTickets.toInt
    }

    map2(optAge, optTickets)(insuranceRateQuote)
  }
}
  
object Option {
  /** Converts a list. If a contains even one None, the result is None, else Some(List(answers)) */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h::t => h.flatMap(hh => sequence(t).map(hh :: _))
  }
  
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}
