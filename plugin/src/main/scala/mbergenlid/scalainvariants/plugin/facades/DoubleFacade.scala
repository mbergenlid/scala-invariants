package mbergenlid.scalainvariants.plugin.facades

import mbergenlid.scalainvariants.annotations.Equal

trait DoubleFacade {

  @Equal("this + x")
  def +(x: Int): Double

  @Equal("this + x")
  def +(x: Double): Double

  @Equal("this + x")
  def +(x: Long): Double

  @Equal("this - x")
  def -(x: Int): Double

  @Equal("this - x")
  def -(x: Double): Double

  @Equal("this - x")
  def -(x: Long): Double

  @Equal("this * x")
  def *(x: Int): Double

  @Equal("this * x")
  def *(x: Double): Double

  @Equal("this * x")
  def *(x: Long): Double
}
