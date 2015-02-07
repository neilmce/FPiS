package fpis

/** Exercises for Chapter 6 of FPinS. */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object StateExercises {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    n match {
      case Int.MinValue => nonNegativeInt(rng2)
      case n if n < 0 => (-n, rng2)
      case _          => (n, rng)
    }
  }
  
  def double(rng: RNG): (Double, RNG) = {
    val (int, nextRng) = nonNegativeInt(rng)
    (int.toDouble/ (Int.MaxValue + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double((rng2))
    ((i, d), rng3)
  }
  
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d,i), rng2)
  }
  
  def double3(rng: RNG): ((Double,Double,Double), RNG) ={
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d1, d2, d3), r4)
  }
  
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng) else {
      val (i, r) = rng.nextInt
      val (tail, finalR) = ints(count - 1)(r)
      (tail, finalR) // TODO Is this the final RNG?
    }
  }
}