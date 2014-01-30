package mbergenlid.tools.boundedintegers
import scala.annotation.StaticAnnotation

case class Bounded(min: Int, max: Int) extends StaticAnnotation
