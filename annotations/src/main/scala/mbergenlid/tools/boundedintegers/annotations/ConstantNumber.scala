package mbergenlid.tools.boundedintegers.annotations
import scala.language.implicitConversions

class ConstantNumber[T: Numeric] protected (value: T)

object ConstantNumber {
  implicit def int2ConstantNumber(value: Int) = new ConstantNumber(value)
  implicit def long2ConstantNumber(value: Long) = new ConstantNumber(value)
  implicit def string2ConstantNumber(value: String) = new ConstantNumber[Int](0)
}
