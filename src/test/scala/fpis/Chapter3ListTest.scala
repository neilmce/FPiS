package fpis

import org.scalatest.FlatSpec

class Chapter3ListTest extends FlatSpec {

  // Exercise 3.13
  "A List" should "be foldLeftable via FoldRight" in {
    val in = List("a", "b", "c")
    val folded = List.foldLeftViaFR(in, "")(_ + _)
    assert(folded === "abc")
  }

  // Exercise 3.14
  "A List" should "be appendable via FoldLeft" in {
    val in = List("a", "b", "c")
    val appended = List.appendViaFR(in, List("d", "e"))
    assert(appended === List("a", "b", "c", "d", "e"))
  }
  
  // Exercise 3.15
  "A List" should "be concatenatable" in {
    val l1 = List("a", "b", "c")
    val l2 = List("d")
    val l3 = List("e", "f")
    val concat = List.concat(List(l1, l2, l3))
    assert(concat === List("a", "b", "c", "d", "e", "f"))
  }

  // Exercise 3.16. map
  "A List" should "be mappable" in {
    val l = List(1,2,3)
    assert(List.map(l)(_ + 1) === List(2,3,4))

  }

  // Exercise 3.19. filter
  "A List" should "be filterable" in {
    val l = List(1,2,3,4,5)
    assert(List.filter(l)(_ % 2 == 0) === List(2,4))
  }

  // Exercise 3.20. flatMap
  "A List" should "be flatmappable" in {
    assert(List.flatMap(List(1,2,3))(i => List(i,i)) === List(1,1,2,2,3,3))
  }
  
  // Exercise 3.21. filter via flatMap
  "A List" should "be filterableViaFlatMap" in {
    val l = List(1,2,3,4,5)
    assert(List.filterViaFM(l)(_ % 2 == 0) === List(2,4))
  }
  
  // Exercise 3.22 & 3.23 zip
  "Lists" should "be zippable" in {
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)
    assert(List.zipWith(l1, l2)(_ + _) === List(5,7,9))
  }
  
  // Exercise 3.24
  "Lists" should "be searchable for subsequences" in {
    val l = List(1,2,3,4)
    assert(List.hasSubsequence(l, List(1,2)) === true)
    assert(List.hasSubsequence(l, List(2,3)) === true)
    assert(List.hasSubsequence(l, List(4)) === true)
    assert(List.hasSubsequence(l, List(1,2,3,4,5)) === false)
  }
}
