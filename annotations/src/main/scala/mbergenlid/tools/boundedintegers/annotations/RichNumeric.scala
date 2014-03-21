package mbergenlid.tools.boundedintegers.annotations

import scala.math.Numeric.IntIsIntegral
import scala.math.Ordering

trait RichNumeric[T] extends Numeric[T] {
  def minValue: T
  def maxValue: T

//  def fromType[U: RichNumeric](value: U): Numeric[T]
}

object RichNumeric {

  trait IntIsRichNumeric extends RichNumeric[Int] with IntIsIntegral {
    override def minValue = Int.MinValue
    override def maxValue = Int.MaxValue
  }

  implicit object IntIsRichNumeric extends IntIsRichNumeric with Ordering.IntOrdering
}
