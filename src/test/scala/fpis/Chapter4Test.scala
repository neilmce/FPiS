package fpis

import org.scalatest.FlatSpec
import fpis.OptionExercises.Try

class Chapter4Test extends FlatSpec {

  "sequence" should "work" in {
    val l1 = List(Some("hi"), Some("hello"), Some("hola"))
    val l2 = List(Some("hi"), None, Some("hola"))

    assert(Option.sequence(l1) === Some(List("hi", "hello", "hola")))
    assert(Option.sequence(l2) === None)
  }
  
  "traverse" should "work" in {
    val l1 = List("1", "123", "789")
    val l2 = List("1", "hello", "22")

    assert(Option.traverse(l1)(s => Try(s.toInt)) === Some(List(1, 123, 789)))
    assert(Option.traverse(l2)(s => Try(s.toInt)) === None)
  }
}
