package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe
import scala.annotation.StaticAnnotation

object BoundedTypeChecker {
  trait BoundedTypeError
  case class Error(message: String) extends BoundedTypeError
  case class Warning(message: String) extends BoundedTypeError

  case class Bounded(min: Int, max: Int) extends StaticAnnotation

}

class BoundedTypeChecker(val global: Universe) {
  import global._
  import BoundedTypeChecker._

  def checkBoundedTypes(tree: Tree): List[BoundedTypeError] = {
    (tree collect treeMatcher).flatten
  }

  def treeMatcher: PartialFunction[Tree, List[BoundedTypeError]] = {
      case Apply(method, args) if(method.symbol.isMethod) => for {
        (argSymbol, paramValue) <- extractMethodParams(method, args) 
        annotation <- argSymbol.annotations.find { a =>
          a.tpe =:= typeOf[Bounded] &&
            !(BoundedInteger(paramValue) <:< BoundedInteger(a))
        }
      } yield { Error("Failure") }
  }

  protected[boundedintegers] 
  def extractMethodParams(methodApplication: Tree, args: List[Tree]): List[(Symbol, Tree)] = {
    val symbol = methodApplication.symbol.asMethod
    symbol.paramss.headOption match {
      case Some(list) => list.zip(args)
      case None => Nil
    }
  }

  class BoundedInteger(private val min: Int = Int.MinValue,
        private val max: Int = Int.MaxValue) {

    def <:<(other: BoundedInteger): Boolean = {
      min >= other.min && max <= other.max
    }
  }

  object BoundedInteger {
    def apply(bounds: Annotation): BoundedInteger = {
      val List(Literal(Constant(min: Int)), Literal(Constant(max: Int))) = bounds.scalaArgs
      new BoundedInteger(min, max)
    }

    def apply(tree: Tree): BoundedInteger = {
      assert(tree.tpe <:< typeOf[Int])
      tree match {
        case Literal(Constant(value: Int)) => new BoundedInteger(value, value)
        case t => 
          val annotationOption = t.symbol.annotations.find( _.tpe =:= typeOf[Bounded])
          annotationOption match {
            case Some(annotation) => this.apply(annotation)
            case None => new BoundedInteger
          }
      }
    }
  }
}
