/**
 * Errors: 21, 27, 42, 60
 */
package compiletests

import mbergenlid.scalainvariants.annotations._

object IfExpressions {

  val anotherRandomInteger = 20

  def testMethod(@GreaterThanOrEqual(0)
                 @LessThanOrEqual(10) a: Int) = a == 3

  def test1() = {
    val x = anotherRandomInteger
    if(x < 11 && -1 < x)
      testMethod(x)
    else
      testMethod(x) //error
  }

  def successfulElse() = {
    val x = anotherRandomInteger
    if(x > 10 || x < 0)
      testMethod(x) //error
    else
      testMethod(x)
  }

  def boundToSymbol() = {
    val maxValue = 10
    def myMethod(@LessThanOrEqual(maxValue)
                 @GreaterThanOrEqual(0L) a: Int, b: String) = 1

    val x = anotherRandomInteger
    if(x > 0 && x < maxValue)
      myMethod(x, "Should compile")

    if(x > 0)
      myMethod(x, "Should not compile") //error
  }

  def boundToSymbolTwoWays() = {
    val x = anotherRandomInteger
    val y = 0

    if(x < 10 && y < x)
      testMethod(x)
  }

  def boundToExpression() = {
    val x = anotherRandomInteger

    if(x > 0 && x + 1 < 12)
      testMethod(x)

    if(x > 0 && x + 2 < 14)
      testMethod(x) //error
  }

  @GreaterThanOrEqual(0)
  def abs1(n: Int) = if(n < 0) 0-n else n //error

  @GreaterThanOrEqual(0)
  def abs2(@GreaterThan(Int.MinValue)n: Int) = if(n < 0) 0-n else n

  @GreaterThanOrEqual(0)
  def abs3(@GreaterThan(Int.MinValue)n: Int) = if(n < 0) -n else n


  // TODO: Add this test when we can express OR as annotations

//  @Or(GreaterThan(10), Equal(-1))
//  def resultOfIfExpression() = {
//    val x = anotherRandomInteger
//
//    if(x > 0) x + 10
//    else -1
//  }
//
//  @Or(GreaterThanOrEqual(0), Equal(-1))
//  def resultOfNegativeIfExpression() = {
//    val x = anotherRandomInteger
//    val y = randomInteger
//
//    if(x == 0) y - x
//    else -1
//  }
//
//  @Or(GreaterThan(10), Equal(-1))
//  def resultOfIfExpressionWithBlock() {
//    val x = anotherRandomInteger
//
//    if(x > 0) {
//      x + 10
//    } else -1
//  }

}