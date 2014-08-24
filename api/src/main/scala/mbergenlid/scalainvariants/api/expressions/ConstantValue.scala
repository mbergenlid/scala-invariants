package mbergenlid.scalainvariants.api.expressions

import mbergenlid.scalainvariants.annotations.RichNumeric

trait ConstantValue extends Ordered[ConstantValue] {
  def value: BigDecimal
  def isOne = value == BigDecimal(1)
  def isZero = value == BigDecimal(0)
  def isGreaterThanZero = value > 0
  def isLessThanZero = value < 0

  def inverse: ConstantValue

  def unary_- : ConstantValue

  override def compare(other: ConstantValue) =
    value.compare(other.value)

  def +(that: ConstantValue) = {
    val newValue = value + that.value
    ConstantValue(newValue)
  }

  def *(that: ConstantValue): ConstantValue = {
    val newValue = value*that.value
    ConstantValue(newValue)
  }

  def ==(other: ConstantValue): Boolean =
    value == other.value


  override def toString = value.toString()
}

class FractionalConstant(val value: BigDecimal) extends ConstantValue {

  def inverse: ConstantValue = ConstantValue(1/value)

  override def unary_- : ConstantValue = ConstantValue(-value)
}

class IntegralConstant(val value: BigDecimal) extends ConstantValue {

  override def inverse: ConstantValue =
    new IntegralConstant(1/value)

  override def unary_- : ConstantValue = ConstantValue(-value)

  override def +(that: ConstantValue) = that match {
    case c: IntegralConstant =>
      ConstantValue(value + c.value)
    case c: FractionalConstant =>
      ConstantValue(value + c.value)
  }

  override def *(that: ConstantValue): ConstantValue = that match {
    case c: IntegralConstant =>
      ConstantValue((value * c.value).toBigInt())
    case c: FractionalConstant =>
      ConstantValue(value * c.value)
  }
}

object ConstantValue {
  def apply[U: RichNumeric](value: U): ConstantValue =
    if(implicitly[RichNumeric[U]].isInstanceOf[Integral[_]])
      new IntegralConstant(implicitly[RichNumeric[U]].toBigDecimal(value))
    else
      new FractionalConstant(implicitly[RichNumeric[U]].toBigDecimal(value))

  def apply(v: BigDecimal): ConstantValue =
    if(v.isWhole()) new IntegralConstant(v)
    else new FractionalConstant(v)

  def apply(v: BigInt): ConstantValue =
    new IntegralConstant(BigDecimal(v))

  lazy val One = ConstantValue(1)
}
