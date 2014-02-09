package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait TypeConstraintValidator { self: MyUniverse =>

  import global._

  implicit class ConstrainedSymbol(symbol: Symbol) {

    def tryAssign(expr: Tree)(implicit context: Context): Option[String] = {
      val boundedAnnotation = symbol.annotations.find(_.tpe =:= typeOf[Bounded])
      if(boundedAnnotation.isDefined) {
        val boundedExpr = getBoundedIntegerFromContext(expr, context)
        val target = BoundedInteger(boundedAnnotation.get)
        if(boundedExpr <:< target) None
        else Some(createErrorMessage(symbol, target, expr, boundedExpr)(context))
      } else {
        None
      }
    }

    private def getBoundedIntegerFromContext(tree: Tree, context: Context) = {
      context(tree.symbol) match {
        case Some(x) => x
        case None => BoundedInteger(tree)
      }
    }

    private def createErrorMessage(targetSymbol: Symbol, targetBounds: BoundedInteger,
                                   assignee: Tree, assigneeBounds: BoundedInteger)
                                 (context: Context): String = {
      val targetName = targetSymbol.name
      val assigneeName = assignee.symbol match {
        case NoSymbol => assignee.toString
        case null => assignee.toString
        case _ => assignee.symbol.name.toString
      }
      s"""
        |Could not assign $assigneeName to $targetName.
        |Unable to prove that:
        |  ${assigneeName} constrained by (${assigneeBounds.constraint.prettyPrint(assigneeName.toString)})
        |is a subtype of
        |  ${targetName} constrained by (${targetBounds.constraint.prettyPrint(targetName.toString)})
      """.stripMargin
    }
  }
}
