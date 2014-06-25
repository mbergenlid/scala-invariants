package mbergenlid.tools.boundedintegers.facades

import mbergenlid.tools.boundedintegers.annotations.{LessThan, GreaterThanOrEqual}

trait ArrayFacade[T] {
  val length: Int

  def apply(@GreaterThanOrEqual(0) @LessThan(length) index: Int): T

  def update(@GreaterThanOrEqual(0) @LessThan(length) index: Int, x: T): Unit
}
