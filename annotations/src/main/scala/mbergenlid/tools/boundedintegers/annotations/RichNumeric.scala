package mbergenlid
package tools
package boundedintegers
package annotations

import scala.math.Numeric.{DoubleIsFractional, LongIsIntegral, IntIsIntegral}
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

  def tryPlus(x: T, y: T): T
  def tryMinus(x: T, y: T): T
  def tryTimes(x: T, y: T): T

  def fromType[U: RichNumeric](value: U): T
}

trait RichInteger[T] extends RichNumeric[T] {
  def toBigInt(x: T): BigInt
  def fromBigInt(x: BigInt): T

  def tryPlus(x: T, y: T): T = tryOp(x,y)(_+_)
  def tryMinus(x: T, y: T): T = tryOp(x,y)(_-_)
  def tryTimes(x: T, y: T): T = tryOp(x,y)(_*_)

  private def tryOp(x: T, y: T)(op: (BigInt, BigInt) => BigInt): T = {
    val result = op(toBigInt(x), toBigInt(y))
    if(result > toBigInt(maxValue) || result < toBigInt(minValue)) throw new ArithmeticException
    else fromBigInt(result)
  }
}

trait RichFractional[T] extends RichNumeric[T] {
  def toBigDecimal(x: T): BigDecimal
  def fromBigDecimal(x: BigDecimal): T

  def tryPlus(x: T, y: T): T = tryOp(x,y)(_+_)
  def tryMinus(x: T, y: T): T = tryOp(x,y)(_-_)
  def tryTimes(x: T, y: T): T = tryOp(x,y)(_*_)

  private def tryOp(x: T, y: T)(op: (BigDecimal, BigDecimal) => BigDecimal): T = {
    val result = op(toBigDecimal(x), toBigDecimal(y))
    if(result > toBigDecimal(maxValue) || result < toBigDecimal(minValue)) throw new ArithmeticException
    else fromBigDecimal(result)
  }
}

object RichNumeric {

  trait IntIsRichNumeric extends IntIsIntegral with RichInteger[Int] {
    override def minValue = Int.MinValue
    override def maxValue = Int.MaxValue
    override def toBigInt(x: Int) = BigInt(x)
    override def fromBigInt(x: BigInt) = x.toInt

    override def fromType[U: RichNumeric](value: U) =
      implicitly[RichNumeric[U]].toInt(value)
  }
  implicit object IntIsRichNumeric extends IntIsRichNumeric with Ordering.IntOrdering

  trait LongIsRichNumeric extends RichInteger[Long] with LongIsIntegral {
    override def minValue = Long.MinValue
    override def maxValue = Long.MaxValue

    override def toBigInt(x: Long) = BigInt(x)
    override def fromBigInt(x: BigInt) = x.toLong

    override def fromType[U: RichNumeric](value: U) =
      implicitly[RichNumeric[U]].toLong(value)
  }
  implicit object LongIsRichNumeric extends LongIsRichNumeric with Ordering.LongOrdering

  trait DoubleIsRichNumeric extends RichFractional[Double] with DoubleIsFractional {
    override def minValue = Double.MinValue
    override def maxValue = Double.MaxValue

    def fromBigDecimal(x: BigDecimal) = x.toDouble
    def toBigDecimal(x: Double) = BigDecimal(x)

    override def fromType[U: RichNumeric](value: U) =
      implicitly[RichNumeric[U]].toDouble(value)
  }

  implicit object DoubleIsRichNumeric extends DoubleIsRichNumeric with Ordering.DoubleOrdering
}
