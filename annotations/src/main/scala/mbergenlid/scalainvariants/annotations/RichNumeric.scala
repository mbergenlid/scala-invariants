package mbergenlid.scalainvariants.annotations

import scala.math.Numeric._
import scala.math.Ordering

/**
 * x*y
 *
 * (lo(x) + hi(x)*b)*(lo(y) + hi(y)*b)
 * (lo(x)*lo(y) + hi(x)*lo(y)*b + hi(y)*lo(x)*b + hi(x)*hi(y)*b*b
 * (lo(x)*lo(y) + hi(x)*lo(y) << b + hi(y)*lo(x) << b + hi(x)*hi(y) << 2*b
 *
 *
 * 32 bit => b = 65536
 */


sealed trait RichNumeric[T] extends Numeric[T] {
  def minValue: T
  def maxValue: T

  def toBigDecimal(x: T): BigDecimal
  def fromBigDecimal(x: BigDecimal): T

  def tryPlus(x: T, y: T): T = tryOp(x,y)(_+_)
  def tryMinus(x: T, y: T): T = tryOp(x,y)(_-_)
  def tryTimes(x: T, y: T): T = tryOp(x,y)(_*_)

  def fromType[U: RichNumeric](value: U): T

  private def tryOp(x: T, y: T)(op: (BigDecimal, BigDecimal) => BigDecimal): T = {
    val result = op(toBigDecimal(x), toBigDecimal(y))
    if(result > toBigDecimal(maxValue) || result < toBigDecimal(minValue)) throw new ArithmeticException
    else fromBigDecimal(result)
  }
}

object RichNumeric {

  trait IntIsRichNumeric extends IntIsIntegral with RichNumeric[Int] {
    override def minValue = Int.MinValue
    override def maxValue = Int.MaxValue
    override def toBigDecimal(x: Int) = BigDecimal(x)
    override def fromBigDecimal(x: BigDecimal) = x.toInt

    override def fromType[U: RichNumeric](value: U) =
      implicitly[RichNumeric[U]].toInt(value)
  }
  implicit object IntIsRichNumeric extends IntIsRichNumeric with Ordering.IntOrdering

  trait LongIsRichNumeric extends RichNumeric[Long] with LongIsIntegral {
    override def minValue = Long.MinValue
    override def maxValue = Long.MaxValue

    override def toBigDecimal(x: Long) = BigDecimal(x)
    override def fromBigDecimal(x: BigDecimal) = x.toLong

    override def fromType[U: RichNumeric](value: U) =
      implicitly[RichNumeric[U]].toLong(value)
  }
  implicit object LongIsRichNumeric extends LongIsRichNumeric with Ordering.LongOrdering

  trait DoubleIsRichNumeric extends RichNumeric[Double] with DoubleIsFractional {
    override def minValue = Double.MinValue
    override def maxValue = Double.MaxValue

    def fromBigDecimal(x: BigDecimal) = x.toDouble
    def toBigDecimal(x: Double) = BigDecimal(x)

    override def fromType[U: RichNumeric](value: U) =
      implicitly[RichNumeric[U]].toDouble(value)
  }

  implicit object DoubleIsRichNumeric extends DoubleIsRichNumeric with Ordering.DoubleOrdering

  trait FloatIsRichNumeric extends RichNumeric[Float] with FloatIsFractional {
    override def minValue = Float.MinValue
    override def maxValue = Float.MaxValue

    def fromBigDecimal(x: BigDecimal) = x.toFloat
    def toBigDecimal(x: Float) = BigDecimal(x)

    override def fromType[U: RichNumeric](value: U) =
      implicitly[RichNumeric[U]].toFloat(value)
  }

  implicit object FloatIsRichNumeric extends FloatIsRichNumeric with Ordering.FloatOrdering

  trait ShortIsRichNumeric extends RichNumeric[Short] with ShortIsIntegral {
    override def minValue = Short.MinValue
    override def maxValue = Short.MaxValue

    def fromBigDecimal(x: BigDecimal) = x.toShort
    def toBigDecimal(x: Short) = BigDecimal(x)

    override def fromType[U: RichNumeric](value: U) =
      fromInt(implicitly[RichNumeric[U]].toInt(value))
  }

  implicit object ShortIsRichNumeric extends ShortIsRichNumeric with Ordering.ShortOrdering
}
