package propertytests

import mbergenlid.scalainvariants.annotations.{LessThan, GreaterThan, GreaterThanOrEqual}

object Test2 {
  @GreaterThanOrEqual(0)
  def testMethod(@GreaterThan(Int.MinValue) n: Int) = if(n < 0) 0-n else n

  @GreaterThan(0)
  def m1(n: Double) = 4

  @LessThan(10)
  def m2(n1: Int, n2: Double) = 4
}