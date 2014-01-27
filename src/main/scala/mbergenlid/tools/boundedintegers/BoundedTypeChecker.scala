package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe

object BoundedTypeChecker {
  trait BoundedTypeResult
  case object Success extends BoundedTypeResult
  case class Error(message: String) extends BoundedTypeResult
}

class BoundedTypeChecker(val global: Universe) {
  import global._
  import BoundedTypeChecker._

  def checkBoundedTypes(tree: Tree): BoundedTypeResult = {
    for (Apply(method, args) <- tree if method.symbol.isMethod) {
        val list = extractMethodParams(method, args)
        println(list)
    }
    Success
  }

  def extractMethodParams(methodApplication: Tree, args: List[Tree]): List[(Symbol, Tree)] = {
    val symbol = methodApplication.symbol.asMethod
    symbol.paramss.headOption match {
      case Some(list) => list.zip(args)
      case None => Nil
    }
  }
}
