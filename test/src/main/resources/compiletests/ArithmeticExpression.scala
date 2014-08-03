
package compiletests

import mbergenlid.scalainvariants.annotations.{GreaterThan, Equal, LessThanOrEqual, GreaterThanOrEqual}

object ArithmeticExpression {

  @GreaterThanOrEqual(0)
  def randomInteger = 4

  def anotherRandomInteger = 20

  def testMethod(@GreaterThanOrEqual(0)
  @LessThanOrEqual(10) a: Int) = a == 3

  @GreaterThanOrEqual(0) @LessThanOrEqual(5)
  val intBetween0And5 = 3
  @GreaterThanOrEqual(5) @LessThanOrEqual(10)
  val intBetween5And10 = 6

  def addConstantToSymbol() = {
    @GreaterThanOrEqual(0)
    @LessThanOrEqual(5)
    val x = intBetween0And5

    val y = x + 4
    testMethod(y)

    testMethod(x + 4)
    testMethod(x + 7) //error
  }

  @GreaterThanOrEqual(0)
  @LessThanOrEqual(5)
  val x = intBetween0And5

  @LessThanOrEqual(5)
  @GreaterThanOrEqual(-5)
  val y = x - 5

  @LessThanOrEqual(1000)
  @GreaterThanOrEqual(-2)
  val z = x - 5 //error

  def multiplyConstantToSymbol() = {
    val x = intBetween0And5

    @GreaterThanOrEqual(0)
    @LessThanOrEqual(50)
    val y = x * 10

    testMethod(x*10) //error
  }

  def addSymbolToSymbol() = {
    val x = intBetween0And5

    @GreaterThanOrEqual(1)
    @LessThanOrEqual(6)
    val y = x + 1
    testMethod(y + x) //error

    @GreaterThanOrEqual(1)
    @LessThanOrEqual(11)
    val z = x + y
  }

  def addConstantToSymbolShouldBeGreaterThanSymbol() = {
    val x = anotherRandomInteger
    val y = anotherRandomInteger

    def myMethod(@GreaterThanOrEqual(x) in: Int) = false
    //should fail because x+1 could overflow
    myMethod(x+1) //error

    if(x < 10)
      myMethod(x+1)

    if(x < 10 && y < 10 && y > 0)
      myMethod(x - y + 11)

    if(x < 10 && y > 0 && y < 10)
      myMethod(x + y + 10)

    val z = x + 1
    true
  }

  def subtractConstantFromSymbolShouldBeLessThanSymbol() = {
    val x = anotherRandomInteger
    val y = anotherRandomInteger

    def myMethod(@LessThanOrEqual(x) in: Int) = false
    //should fail because x-1 could underflow
    myMethod(x-1) //error

    if(x > 0)
      myMethod(x-1)

    if(x > 0 && y < 10 && y > 0)
      myMethod(x - y - 11)

    if(x > 0 && y > 0 && y < 10)
      myMethod(x + y - 10)

    val z = x - 1
    true
  }

  def shouldNotBeAbleToCancelOutMethods() = {
    val x = anotherRandomInteger
    val y = anotherRandomInteger
    var v1 = anotherRandomInteger
    val x1 = v1
    val y1 = v1

    @Equal(1)
    val a = x - y + 1 //error

    @Equal(1)
    val b = x - x + 1

    @Equal(1)
    val c = x1 - y1 + 1 //error

    a+b+c
  }

  def multiplySymbolsCouldOverflow() = {
    val x = randomInteger
    val y = anotherRandomInteger

    @GreaterThanOrEqual(0)
    val a = x*y //error

    a
  }

  def addToSymbolCouldOverflow() = {
    val x = anotherRandomInteger

    @GreaterThan(Int.MinValue)
    val y = x + 1 //error
    y
  }

}