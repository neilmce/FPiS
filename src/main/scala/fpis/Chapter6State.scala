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
}