package mbergenlid.tools.boundedintegers.facades

import mbergenlid.tools.boundedintegers.annotations.{LessThan, GreaterThanOrEqual}

trait ArrayFacade[T] {
  @(GreaterThanOrEqual @scala.annotation.meta.getter)(0)
  val length: Int

  def apply(@GreaterThanOrEqual(0) @LessThan(length) index: Int): T

  def update(@GreaterThanOrEqual(0) @LessThan(length) index: Int, x: T): Unit
}
