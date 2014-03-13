package mbergenlid.tools.boundedintegers
import scala.annotation.StaticAnnotation

sealed trait BoundedType extends StaticAnnotation
case class Bounded(min: Int, max: Int) extends BoundedType

case class LessThan(value: Int) extends StaticAnnotation

case class Equal(value: Int) extends BoundedType
case class GreaterThanOrEqual(value: Int) extends BoundedType
case class LessThanOrEqual(value: Int) extends BoundedType