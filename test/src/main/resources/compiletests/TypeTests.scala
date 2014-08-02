package compiletests

import mbergenlid.tools.boundedintegers.annotations.{GreaterThan, GreaterThanOrEqual, LessThanOrEqual, LessThan}

import scala.util.Random

object TypeTests {

  @LessThanOrEqual(5)
  @GreaterThanOrEqual(0)
  def intBetween0And5 = 4

  def anotherRandomInteger = new Random().nextInt()

  def simpleIntPlusDouble() = {
    val x = intBetween0And5

    @LessThan(6)
    val y = x + 0.5

    y
  }

  def increment() = {
    @LessThanOrEqual(9)
    def intMethod(@LessThan(10) x: Int) = x

    @LessThanOrEqual(9)
    def method(@LessThan(10) x: Double) = x //error

    val x: Double = anotherRandomInteger
    if(x < 10) {
      @LessThanOrEqual(10)
      val y = 1 + x //error
      y
    }
  }

  def decrement() = {
    @GreaterThanOrEqual(10)
    def intMethod(@GreaterThan(9) x: Int) = x

    @GreaterThanOrEqual(10)
    def method(@GreaterThan(9) x: Double) = x //error

    val x: Double = anotherRandomInteger
    if(x > 10) {
      @GreaterThanOrEqual(9)
      val y = 1 + x //error
      y
    }
  }
}