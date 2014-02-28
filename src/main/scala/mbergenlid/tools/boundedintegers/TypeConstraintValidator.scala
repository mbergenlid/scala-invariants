package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait TypeConstraintValidator extends AbstractBoundsValidator {
  self: MyUniverse =>

  import global._

  implicit class ConstrainedSymbol(symbol: Symbol) {

    def tryAssign(expr: Tree)(implicit context: Context): BoundedInteger = {
      val boundedAnnotation = symbol.annotations.find(_.tpe =:= typeOf[Bounded])
      val boundedExpr = checkBounds(context)(expr)
      if(boundedAnnotation.isDefined) {
        val target = BoundsFactory(boundedAnnotation.get)
//        val pretty = boundedExpr.constraint.prettyPrint("x")
//        println(s"$symbol = $pretty")
        if(!(boundedExpr <:< target)) 
          reportError(Error(expr.pos, createErrorMessage(symbol, target, expr, boundedExpr)(context)))
      } 
      boundedExpr
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
