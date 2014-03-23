package mbergenlid.tools.boundedintegers.annotations
import scala.annotation.StaticAnnotation
import scala.language.existentials

sealed trait Bounded extends StaticAnnotation

case class LessThan(value: ConstantNumber[_]) extends Bounded
case class Equal(value: ConstantNumber[_]) extends Bounded
case class GreaterThanOrEqual(value: ConstantNumber[_]) extends Bounded
case class LessThanOrEqual(value: ConstantNumber[T] forSome { type T }) extends Bounded