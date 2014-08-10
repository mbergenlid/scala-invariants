package mbergenlid.scalainvariants.plugin.facades

import mbergenlid.scalainvariants.annotations.Equal

trait FloatFacade {

  @Equal("this + x")
  def +(x: Short): Float

  @Equal("this + x")
  def +(x: Int): Float

  @Equal("this + x")
  def +(x: Double): Double

  @Equal("this + x")
  def +(x: Float): Float

  @Equal("this + x")
  def +(x: Long): Float

  @Equal("this - x")
  def -(x: Short): Float

  @Equal("this - x")
  def -(x: Int): Float

  @Equal("this - x")
  def -(x: Double): Double

  @Equal("this - x")
  def -(x: Float): Float

  @Equal("this - x")
  def -(x: Long): Float

  @Equal("this * x")
  def *(x: Short): Float

  @Equal("this * x")
  def *(x: Int): Float

  @Equal("this * x")
  def *(x: Double): Double

  @Equal("this * x")
  def *(x: Float): Float

  @Equal("this * x")
  def *(x: Long): Float
}
