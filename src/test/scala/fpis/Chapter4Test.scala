package fpis

import org.scalatest.FlatSpec

class Chapter4Test extends FlatSpec {

  "sequence" should "work" in {
    val l1 = List(Some("hi"), Some("hello"), Some("hola"))
    val l2 = List(Some("hi"), None, Some("hola"))

    assert(Exercises.sequence(l1) === Some(List("hi", "hello", "hola")))
    assert(Exercises.sequence(l2) === None)
  }
}
