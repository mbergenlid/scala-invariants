package mbergenlid.tools.boundedintegers.annotations
import scala.annotation.StaticAnnotation
import scala.language.existentials

sealed trait BoundedType extends StaticAnnotation
case class Bounded(min: Int, max: Int) extends BoundedType

case class LessThan(value: Int) extends StaticAnnotation

case class Equal(value: ConstantNumber[_]) extends BoundedType
case class GreaterThanOrEqual(value: ConstantNumber[_]) extends BoundedType
case class LessThanOrEqual(value: ConstantNumber[T] forSome { type T }) extends BoundedType