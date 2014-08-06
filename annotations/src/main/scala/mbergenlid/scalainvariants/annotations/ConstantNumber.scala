package mbergenlid.scalainvariants.annotations
import scala.language.implicitConversions

trait ConstraintExpression
class ConstantNumber[T: Numeric] protected (value: T) extends ConstraintExpression

object ConstantNumber {
  implicit def int2ConstantNumber(value: Int) = new ConstantNumber(value)
  implicit def long2ConstantNumber(value: Long) = new ConstantNumber(value)
  implicit def double2ConstantNumber(value: Double) = new ConstantNumber(value)
  implicit def string2ConstantNumber(value: String) = new ConstantNumber[Int](0)
}
