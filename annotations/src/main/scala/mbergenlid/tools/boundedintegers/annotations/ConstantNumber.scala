package mbergenlid.tools.boundedintegers.annotations
import scala.language.implicitConversions

class ConstantNumber[T: Numeric] private (value: T)

object ConstantNumber {
  implicit def int2ConstantNumber(value: Int) = new ConstantNumber(value)
  implicit def long2ConstantNumber(value: Long) = new ConstantNumber(value)
}
