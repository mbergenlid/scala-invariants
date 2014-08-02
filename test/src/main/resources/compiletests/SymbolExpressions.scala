/**
 * Errors: 22, 31
 */
package compiletests

import mbergenlid.tools.boundedintegers.annotations.{LessThanOrEqual, GreaterThanOrEqual}

object SymbolExpressions {
  val anotherRandomInteger = 20

  def testMethod(@GreaterThanOrEqual(0)
                 @LessThanOrEqual(10) a: Int) = a == 3

  @GreaterThanOrEqual(0) @LessThanOrEqual(5)
  val intBetween0And5 = 3
  @GreaterThanOrEqual(5) @LessThanOrEqual(10)
  val intBetween5And10 = 6
  def boundSymbolToExpression() = {
    val x = anotherRandomInteger

    if(x > 0 && x + intBetween0And5 < 15)
      testMethod(x)  //error

    if(x > 0 && x + intBetween5And10 < 15)
      testMethod(x)

    if(x < 11 && x + intBetween5And10 > 10)
      testMethod(x)

    if(x < 11 && x + intBetween0And5 > 2)
      testMethod(x)  //error
  }

  def boundWithLessOrEqual() = {
    val x = anotherRandomInteger
    val y = intBetween0And5

    if(x > 0 && x + intBetween0And5 <= 15)
      testMethod(x)  //error

    if(x > 0 && x + intBetween5And10 <= 15)
      testMethod(x)

    if(x < 11 && x + intBetween5And10 >= 10)
      testMethod(x)

    if(x < 11 && x + intBetween0And5 >= 2)
      testMethod(x)  //error
  }

  def boundWithEqual() = {
    val x = anotherRandomInteger
    val y = intBetween0And5

    if(x + intBetween0And5 == 15)
      testMethod(x)  //error

    if(x + intBetween0And5 == 10)
      testMethod(x)
  }
}