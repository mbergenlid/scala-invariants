package propertytests

import mbergenlid.scalainvariants.annotations.GreaterThanOrEqual

object Test1 {

  @GreaterThanOrEqual(0)
  def testMethod(n: Int) = 5

  @GreaterThanOrEqual(0)
  def m2(a1: Int,a2: Int,a3: Int,a4: Int,a5: Int,a6: Int,a7: Int,a8: Int) = 1

  @GreaterThanOrEqual(0)
  def m3 = 1
}