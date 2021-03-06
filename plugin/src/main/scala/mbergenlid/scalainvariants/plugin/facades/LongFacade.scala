package mbergenlid.scalainvariants.plugin.facades

import mbergenlid.scalainvariants.annotations.Equal

trait LongFacade {

  @Equal("this + x")
  def +(x: Short): Long

  @Equal("this + x")
  def +(x: Int): Long

  @Equal("this + x")
  def +(x: Double): Double

  @Equal("this + x")
  def +(x: Float): Float

  @Equal("this + x")
  def +(x: Long): Long

  @Equal("this - x")
  def -(x: Short): Long

  @Equal("this - x")
  def -(x: Int): Long

  @Equal("this - x")
  def -(x: Double): Double

  @Equal("this - x")
  def -(x: Float): Float

  @Equal("this - x")
  def -(x: Long): Long

  @Equal("this * x")
  def *(x: Short): Long

  @Equal("this * x")
  def *(x: Int): Long

  @Equal("this * x")
  def *(x: Long): Long

  @Equal("this * x")
  def *(x: Double): Double

  @Equal("this * x")
  def *(x: Float): Float
}
