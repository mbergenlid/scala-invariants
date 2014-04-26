package mbergenlid.tools.test

import mbergenlid.tools.boundedintegers.annotations.{LessThan, GreaterThanOrEqual}

object Test1 {

  class SafeArray(@GreaterThanOrEqual(0) length: Int) {
    val backingArray: Array[Int] = new Array(length)
    def apply(@GreaterThanOrEqual(0)
              @LessThan(length) index: Int) = backingArray(index)
  }

  new SafeArray(-2)
}
