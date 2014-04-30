package test

import mbergenlid.tools.boundedintegers.annotations.GreaterThanOrEqual

object Test2 {
  @GreaterThanOrEqual(0)
  def testMethod(n: Int) = if(n < 0) 0-n else n
}