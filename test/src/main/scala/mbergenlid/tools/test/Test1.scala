package mbergenlid.tools.test

import mbergenlid.tools.boundedintegers.annotations.{Equal, LessThan, GreaterThanOrEqual}

object Test1 {

  class SafeArray(@GreaterThanOrEqual(0) length: Int) {
    val backingArray: Array[Int] = new Array(length)
    def apply(@GreaterThanOrEqual(0)
              @LessThan(length) index: Int) = backingArray(index)
  }

  @GreaterThanOrEqual(0)
  def testMethod(n: Int) = math.abs(n)
}
