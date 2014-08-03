package compiletests

import mbergenlid.scalainvariants.annotations.{LessThanOrEqual, LessThan, GreaterThanOrEqual}

object ArraySearchTest {

  def linearSearch() = {
    @GreaterThanOrEqual(-1)
    @LessThan("array.length")
    def find(
      array: Array[Int],
      element: Int,
      @GreaterThanOrEqual(0) @LessThanOrEqual("array.length") start: Int): Int = {

      if(start == array.length) -1
      else if(array(start) == element) start
      else find(array, element, start+1)
    }

    val sa = Array(1,2,3,4,5)
    val index = find(sa, 4, 0)

    if(index > -1) sa(index)
    else index
  }
}