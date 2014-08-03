package compiletests

import mbergenlid.tools.boundedintegers.annotations._

import scala.util.Random

object ArithmeticUnitTests {

  @LessThanOrEqual(5)
  @GreaterThanOrEqual(0)
  def intBetween0And5 = 4

  @GreaterThanOrEqual(0)
  def randomInteger = 20

  def subtractSymbolFromConstant() = {
    val x = intBetween0And5

    @LessThan(11)
    @GreaterThan(4)
    val ok = 10 - x

    @LessThanOrEqual(5)
    val notOk = 10 - x  //error
  }

  @GreaterThanOrEqual(0)
  @LessThanOrEqual(10)
  def multiplySymbolToConstant() = {
    val x = intBetween0And5
    2*x
  }

// TODO: Add this test when we can express OR as annotations
//  @Or(Equal(9), GreaterThanOrEqual(10))
//  def addToIfExpression() = {
//    val x = new Random().nextInt()
//    10 + (if(x < 0) -1 else x)
//  }

  @GreaterThanOrEqual(0)
  @LessThanOrEqual(Int.MaxValue)
  def multiplySymbolToOtherSymbol() = { //error
    val x = randomInteger
    val y = new Random().nextInt()

    x*y
  }

  @GreaterThanOrEqual(0)
  @LessThanOrEqual(25)
  def multiplyBoundedSymbols() = {
    val x = intBetween0And5
    val y = intBetween0And5

    x*y
  }

}