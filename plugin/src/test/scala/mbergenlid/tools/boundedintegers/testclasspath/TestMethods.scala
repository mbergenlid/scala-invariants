package mbergenlid.tools.boundedintegers.testclasspath

import mbergenlid.tools.boundedintegers.annotations._
import mbergenlid.tools.boundedintegers.annotations.ConstantNumber._
import mbergenlid.tools.boundedintegers.annotations.GreaterThanOrEqual
import mbergenlid.tools.boundedintegers.annotations.LessThanOrEqual
import mbergenlid.tools.boundedintegers.annotations.LessThan

object TestMethods {

  var globalVariable = 5

  def testMethod( @GreaterThanOrEqual(0)
                  @LessThanOrEqual(10) a: Int) = a == 3

  def upperBoundMethod(@LessThanOrEqual(10)a: Int) = a == 1
  
  @GreaterThanOrEqual(0)
  def randomInteger = 4

  @GreaterThanOrEqual(1)
  @LessThanOrEqual(10)
  def intBetween0And10 = 4
  @LessThanOrEqual(5)
  @GreaterThanOrEqual(0)
  def intBetween0And5 = 4

  @LessThanOrEqual(10)
  @GreaterThanOrEqual(5)
  def intBetween5And10 = 6

  def anotherRandomInteger = 20

  @Property("length", Equal(5))
  def fiveCharacterString = "12345"

  class SafeArray(@GreaterThanOrEqual(0) val length: Int) {

    val backingArray: Array[Int] = new Array(length)
    def apply( @GreaterThanOrEqual(0)
               @LessThan(length) index: Int): Int = backingArray(index)
  }

}
