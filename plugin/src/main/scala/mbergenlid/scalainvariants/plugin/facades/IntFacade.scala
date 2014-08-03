package mbergenlid.scalainvariants.plugin.facades

import mbergenlid.scalainvariants.annotations.{ConstantNumber, Equal}

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

