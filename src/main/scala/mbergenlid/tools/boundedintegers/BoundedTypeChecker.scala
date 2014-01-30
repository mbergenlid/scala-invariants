package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe
import scala.language.implicitConversions

trait MyUniverse {
  val global: Universe

  trait BoundedTypeError
  case class Error(message: String) extends BoundedTypeError
  case class Warning(message: String) extends BoundedTypeError


  import global._
  case class BoundedInteger(min: Int = Int.MinValue, max: Int = Int.MaxValue) {

    def <:<(other: BoundedInteger): Boolean = {
      min >= other.min && max <= other.max
    }

    def <|(other: BoundedInteger) =
      this.copy(max = Math.min(max, other.max))
    
    def >|(other: BoundedInteger) =
      new BoundedInteger(Math.max(min, other.min), max)
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

    implicit def integerToBounded(x: Int) = BoundedInteger(x, x)
  }
}

trait Context { }
object BoundedTypeChecker {

}


class BoundedTypeChecker(val global: Universe) extends MyUniverse {
  import global._
  import BoundedTypeChecker._

  def checkBoundedTypes(tree: Tree): List[BoundedTypeError] = {
    checkTree(tree)
  }

  def checkTree(tree: Tree): List[BoundedTypeError] = tree match {
      case Apply(method, args) if(method.symbol.isMethod) => (for {
        (argSymbol, paramValue) <- extractMethodParams(method, args) 
        annotation <- argSymbol.annotations.find { a =>
          a.tpe =:= typeOf[Bounded] &&
            !(BoundedInteger(paramValue) <:< BoundedInteger(a))
        }
      } yield { Error("Failure") }) ++ checkTree(method)
      //case If(cond, _then, _else) => 
        //Parse cond
        //checkTree(_then) ++ checkTree(_else)
      case _ => (for {
        child <- tree.children
      } yield (checkTree(child))).flatten
  }

  protected[boundedintegers] 
  def extractMethodParams(methodApplication: Tree, args: List[Tree]): List[(Symbol, Tree)] = {
    val symbol = methodApplication.symbol.asMethod
    symbol.paramss.headOption match {
      case Some(list) => list.zip(args)
      case None => Nil
    }
  }

}
