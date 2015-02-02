package fpis

import org.scalatest.FlatSpec

class Chapter3TreeTest extends FlatSpec {

  // Exercise 3.25
  "A Tree" should "be sizeable" in {
    assert(Tree.size(Leaf("a")) === 1)
    assert(Tree.size(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c")))) === 5)
  }

  val numberTree = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(5)), Branch(Leaf(4), Leaf(3))))
  
  // Exercise 3.26
  "A tree of numbers" should "be maxable" in {
    assert(Tree.maximum(numberTree) === 5)
  }
  
  // Exercise 3.27
  "A tree" should "be depthable" in {
    assert(Tree.depth(numberTree) === 4)
  }
  
  // Exercise 3.28
  "A tree" should "be mappable" in {
    val t1 = Branch(Leaf("hello"), Branch(Leaf("hi"), Leaf("hola")))
    val t2 = Branch(Leaf(5),       Branch(Leaf(2   ), Leaf(4)))
    assert(Tree.map(t1)(l => l.length()) === t2)
  }
}
