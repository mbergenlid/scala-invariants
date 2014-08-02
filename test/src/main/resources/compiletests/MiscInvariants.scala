package compiletests

import mbergenlid.tools.boundedintegers.annotations._

object MiscInvariants {

  def testMethod( @GreaterThanOrEqual(0)
                  @LessThanOrEqual(10) a: Int) = a == 3

  @GreaterThanOrEqual(0)
  def randomInteger = 4

  def anotherRandomInteger = 20

  def upperBoundMethod(@LessThanOrEqual(10)a: Int) = a == 1
  def lowerBoundMethod(@GreaterThanOrEqual(0)a: Int) = a == 1

  def failureWithConstantArgument() = {
    testMethod(4)
    testMethod(11) //error
    testMethod(-1) //error
  }

  def failureWithSimpleVal() = {
    val x = 11
    testMethod(x) //error

    @Equal(10) val y = 10
    testMethod(y)

    testMethod(anotherRandomInteger) //error
    val z = anotherRandomInteger
    testMethod(z) //error
  }

  def validationInMiddleOfBooleanExpression() = {
    val x = randomInteger

    if(x < 10 && testMethod(x)) println("Should compile")
    if(x > 0 && testMethod(x)) println("Should not compile") //error
  }

  def implicitlyBoundToMethod() = {
    val x = 5
    @Equal(x)
    val y = x

    testMethod(y)
  }

  def boundConstantToSymbol() = {
    val x = anotherRandomInteger
    def upperBound(@LessThan(x)a: Int) = true
    def upperBoundInclusive(@LessThanOrEqual(x)a: Int) = true
    def lowerBound(@GreaterThan(x)a: Int) = true
    def lowerBoundInclusive(@GreaterThanOrEqual(x)a: Int) = true
    def equal(@Equal(x)a: Int) = true

    if(x > 3) upperBound(2)
    if(x < 3) upperBound(2) //error
    if(x > 1) upperBound(2) //error
    if(x > 2) upperBound(2)
    if(x < 2) upperBound(2) //error

    if(x < 1) lowerBound(2)
    if(x > 1) lowerBound(2) //error
    if(x < 3) lowerBound(2) //error
    if(x < 2) lowerBound(2)
    if(x > 2) lowerBound(2) //error

    if(x > 3) upperBoundInclusive(2)
    if(x < 3) upperBoundInclusive(2) //error
    if(x > 1) upperBoundInclusive(2)
    if(x > 2) upperBoundInclusive(2)
    if(x < 2) upperBoundInclusive(2) //error

    if(x < 1) lowerBoundInclusive(2)
    if(x > 1) lowerBoundInclusive(2) //error
    if(x < 3) lowerBoundInclusive(2)
    if(x < 2) lowerBoundInclusive(2)
    if(x > 2) lowerBoundInclusive(2) //error
  }

  def boundConstantToSymbolWithInclusiveConstraints() = {
    val x = anotherRandomInteger
    def upperBound(@LessThan(x)a: Int) = true
    def upperBoundInclusive(@LessThanOrEqual(x)a: Int) = true
    def lowerBound(@GreaterThan(x)a: Int) = true
    def lowerBoundInclusive(@GreaterThanOrEqual(x)a: Int) = true
    def equal(@Equal(x)a: Int) = true

    if(x >= 3) upperBound(2)
    if(x <= 3) upperBound(2) //error
    if(x >= 1) upperBound(2) //error
    if(x >= 2) upperBound(2)
    if(x <= 2) upperBound(2) //error

    if(x <= 1) lowerBound(2)
    if(x >= 1) lowerBound(2) //error
    if(x <= 3) lowerBound(2) //error
    if(x <= 2) lowerBound(2)
    if(x >= 2) lowerBound(2) //error

    if(x >= 3) upperBoundInclusive(2)
    if(x <= 3) upperBoundInclusive(2) //error
    if(x >= 1) upperBoundInclusive(2) //error
    if(x >= 2) upperBoundInclusive(2)
    if(x <= 2) upperBoundInclusive(2) //error

    if(x <= 1) lowerBoundInclusive(2)
    if(x >= 1) lowerBoundInclusive(2) //error
    if(x <= 3) lowerBoundInclusive(2) //error
    if(x <= 2) lowerBoundInclusive(2)
    if(x >= 2) lowerBoundInclusive(2) //error
  }

  def boundConstantToSymbolTransitively() = {
    val x = anotherRandomInteger
    val y = anotherRandomInteger
    def lowerBound(@GreaterThan(x) n: Int) = true

    if(x < 10 && y > 10)
      lowerBound(y)
    if(x < 10 && y == 10)
      lowerBound(y)
    if(x < 10 && y <= 10)
      lowerBound(y) //error
  }

  def symbolChains() = {
    object T1 {
      @Equal(1)
      val x = 1
    }
    object T2 {
      val x = 2
    }

    @Equal(11)
    val y = T1.x + 10
    @Equal(1)
    val z = T2.x //error
    y + z
  }

  def symbol() = {
    if(randomInteger < 10) {
      testMethod(randomInteger) //error
    }

    val x = anotherRandomInteger
    if(randomInteger < x) {
      lowerBoundMethod(x)
    }

    lowerBoundMethod(randomInteger)

    if(randomInteger < 10 && x < randomInteger) {
      upperBoundMethod(x) //error
    }
  }

  def scopeTest() = {
    val x = randomInteger

    val y = if(x < 10) {
      val x = 5
      x
    } else x

    @Equal(x)
    val z = y //error

    z
  }
}