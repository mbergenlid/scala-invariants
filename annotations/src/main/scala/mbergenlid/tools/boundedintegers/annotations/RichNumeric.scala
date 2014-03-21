package mbergenlid
package tools
package boundedintegers
package annotations

import scala.math.Numeric.{LongIsIntegral, IntIsIntegral}
import scala.math.Ordering

sealed trait RichNumeric[T] extends Numeric[T] {
  def minValue: T
  def maxValue: T

  def fromType[U: RichNumeric](value: U): T
}

object RichNumeric {

  trait IntIsRichNumeric extends RichNumeric[Int] with IntIsIntegral {
    override def minValue = Int.MinValue
    override def maxValue = Int.MaxValue

    override def fromType[U: RichNumeric](value: U) =
      implicitly[RichNumeric[U]].toInt(value)
  }
  implicit object IntIsRichNumeric extends IntIsRichNumeric with Ordering.IntOrdering

  trait LongIsRichNumeric extends RichNumeric[Long] with LongIsIntegral {
    override def minValue = Long.MinValue
    override def maxValue = Long.MaxValue

    override def fromType[U: RichNumeric](value: U) =
      implicitly[RichNumeric[U]].toLong(value)
  }
  implicit object LongIsRichNumeric extends LongIsRichNumeric with Ordering.LongOrdering
}
