package co.com.scalatraining.tuples

import org.scalatest.FunSuite

class TupleSuite  extends FunSuite {

  test("Una tupla se debe poder crear"){
    val tupla: (Int, Int, String, List[Int]) = (1, 2,"3", List(1, 2, 3))
    assert(tupla._2 == 2)
    assert(tupla._4.tail.head == 2)
  }

}
