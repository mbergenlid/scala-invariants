package mbergenlid.tools.boundedintegers.testclasspath

import mbergenlid.tools.boundedintegers.annotations.{ConstantNumber, LessThanOrEqual, LessThan, GreaterThanOrEqual}
import mbergenlid.tools.boundedintegers.annotations.ConstantNumber._

object TestMethods {
  import mbergenlid.tools.boundedintegers.annotations.Bounded

  var globalVariable = 5
//  @Bounded(min=0, max=10)
//  var globalBoundedVariable = 5
//  @Bounded(min=0, max=10)
//  var globalBoundedValue = 5

  
  def testMethod( @GreaterThanOrEqual(0)
                  @LessThanOrEqual(10) a: Int) = a == 3

  def upperBoundMethod(@LessThanOrEqual(10)a: Int) = a == 1
  
  @GreaterThanOrEqual(0)
  def randomInteger = 4

  @GreaterThanOrEqual(1)
  @LessThanOrEqual(0)
  def intBetween0And10 = 4
  @LessThanOrEqual(5)
  @GreaterThanOrEqual(1)
  def intBetween0And5 = 4
  def anotherRandomInteger = 20
}
