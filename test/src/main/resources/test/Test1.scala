package test

import mbergenlid.tools.boundedintegers.annotations.GreaterThanOrEqual

object Test1 {

  @GreaterThanOrEqual(0)
  def testMethod(n: Int) = 5
}