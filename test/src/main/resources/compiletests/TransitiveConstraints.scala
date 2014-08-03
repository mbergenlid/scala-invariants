package compiletests

import mbergenlid.scalainvariants.annotations.{GreaterThanOrEqual, LessThanOrEqual}

import scala.util.Random

object TransitiveConstraints {
  def randomInteger = new Random().nextInt()

  @LessThanOrEqual(10)
  @GreaterThanOrEqual(5)
  def intBetween5And10 = 6

  @LessThanOrEqual(10)
  @GreaterThanOrEqual(0)
  def intBetween0And10 = 4

  def testMethod( @GreaterThanOrEqual(0)
  @LessThanOrEqual(10) a: Int) = a == 3

  def anotherRandomInteger = 20

  def transitiveConstraints() = {
    val x = intBetween0And10
    val y = anotherRandomInteger
    val z = anotherRandomInteger

    if(y < x) {
      if(z > 0 && z < y) testMethod(z)
    }
  }

  def transitiveInSameBooleanExpression() = {
    val x = intBetween0And10
    val y = anotherRandomInteger
    val z = anotherRandomInteger

    if(y < x && z > 0 && z < y) testMethod(z)
  }

  def deferredTransitiveConstraint() = {
    val x = anotherRandomInteger
    val y = anotherRandomInteger

    if(x > 0 && x < y) {
      if(y < 10) testMethod(x)
    }
  }

  def cyclicTransitiveConstraint() = {
    val x = anotherRandomInteger
    val y = anotherRandomInteger

    if(x > 0 && x < y) {
      if(y < x) testMethod(x) //error
    }
  }
}