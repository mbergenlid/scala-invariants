package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe
import scala.annotation.StaticAnnotation

object BoundedTypeChecker {
  trait BoundedTypeError
  case object Success extends BoundedTypeError
  case class Error(message: String) extends BoundedTypeError

  case class Bounded(min: Int, max: Int) extends StaticAnnotation
}

class BoundedTypeChecker(val global: Universe) {
  import global._
  import BoundedTypeChecker._

  def checkBoundedTypes(tree: Tree): List[BoundedTypeError] = {
    for {
      Apply(method, args) <- tree
      if method.symbol.isMethod
      (argSymbol, paramValue) <- extractMethodParams(method, args) 
      annotation <- argSymbol.annotations
      if(annotation.tpe =:= typeOf[Bounded])
      error <- validate(annotation, paramValue)
    } yield {
      error
    }
  }

  private def validate(bounds: Annotation, appliedParam: Tree): Option[BoundedTypeError] = appliedParam match {
    case Literal(Constant(value: Int)) => bounds.scalaArgs match {
      case List(Literal(Constant(min: Int)), Literal(Constant(max: Int))) if (value < min || value > max) => Some(Error("Failure"))
      case _ => None
    }
    case _ => None
  }

  protected[boundedintegers] def extractMethodParams(methodApplication: Tree, args: List[Tree]): List[(Symbol, Tree)] = {
    val symbol = methodApplication.symbol.asMethod
    symbol.paramss.headOption match {
      case Some(list) => list.zip(args)
      case None => Nil
    }
  }
}
