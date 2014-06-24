package mbergenlid.tools.boundedintegers.facades

import mbergenlid.tools.boundedintegers.annotations.{ConstantNumber, Equal}

trait IntFacade {

  @Equal("this + x")
  def +(x: Int): Int

  @Equal("this + x")
  def +(x: Double): Double

  @Equal("this - x")
  def -(x: Int): Int

  @Equal("this * x")
  def *(x: Int): Int
}

