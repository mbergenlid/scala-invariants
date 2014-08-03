package compiletests

import mbergenlid.scalainvariants.annotations.{LessThanOrEqual, GreaterThanOrEqual}

import scala.util.Random

/**
 * Note: Variables have very limited support at the moment.
 * They are not tracked in the context, but they can have bounds,
 * in which way they behave very much like methods.
 */
object VarTests {

  def testMethod( @GreaterThanOrEqual(0)
                  @LessThanOrEqual(10) a: Int) = a == 3

  def anotherRandomInteger = new Random().nextInt()
  def simpleVar() = {
    var x = 5
    testMethod(x) //error

    @LessThanOrEqual(10)
    @GreaterThanOrEqual(0)
    var y = 11 //error

    x = 1
    y = 2
    x+y
  }

  def simpleVarBoundary() = {
    @LessThanOrEqual(10)
    @GreaterThanOrEqual(0)
    var x = 5

    val y = anotherRandomInteger
    if(y > 0 && y < x) testMethod(y)

    x = 3
  }

  def reAssigningABoundedVariable() = {
    @GreaterThanOrEqual(0)
    @LessThanOrEqual(10)
    var x = 5
    x = 100 //error
    x = 9
    x
  }

  def modifyingABoundedVariable() = {
    @GreaterThanOrEqual(0)
    @LessThanOrEqual(10)
    var x = 5
    x = x + 2 //error
    x
  }

  def modifyingVarAfterCheck() = {
    var x = 5
    def boundToVar(@GreaterThanOrEqual(0) @LessThanOrEqual(x)a: Int) = a == 1

    val y = anotherRandomInteger
    if(y > 0 && y < x) {
      x -= 2
      boundToVar(y) //error
    }
  }
}