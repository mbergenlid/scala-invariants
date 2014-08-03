package compiletests

import mbergenlid.scalainvariants.annotations.{Equal, LessThanOrEqual}

object MethodDefinition {
  @LessThanOrEqual(10)
  def invalidMethod1 = 11 //error

  @LessThanOrEqual(10)
  def validMethod1 = 5

  @Equal(11)
  val x = 11

  @LessThanOrEqual(10)
  def invalidMethod2 = x //error

  @LessThanOrEqual(10)
  def validMethod2 = x - 1
}