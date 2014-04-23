package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait TypeConstraintValidator extends AbstractBoundsValidator {
  self: MyUniverse =>

  import global._

  implicit def symbol2ConstrainedSymbol(symbol: Symbol) =
    new ConstrainedSymbol(symbol)

  class ConstrainedSymbol(symbol: Symbol, thisSymbol: Option[Symbol] = None) {
    def this(symbol: Symbol, thisSymbol: Symbol) = this(symbol, Some(thisSymbol))

    def withThisSymbol(withThisSymbol: Symbol) = new ConstrainedSymbol(symbol, withThisSymbol)
    
    def tryAssign(expr: Tree)(implicit context: Context): BoundedType = {
      if(expressionForType.isDefinedAt(symbol.typeSignature)) {
        val target =
          for(sc <- BoundsFactory.apply(symbol, symbol.typeSignature)) yield replaceThisSymbols(sc)

        val boundExpr = checkBounds(context)(expr)
        val exprConstraints = Context.getConstraint(boundExpr.constraint, symbol.typeSignature, context)
        val assignee =
          exprConstraints && Context.substituteConstants(exprConstraints, symbol.typeSignature, context)
        if(!(assignee obviouslySubsetOf target))
          reportError(Error(expr.pos, createErrorMessage(symbol, target, expr, assignee)(context)))
        boundExpr
      } else {
        checkBounds(context)(expr)
      }
    }

    private def replaceThisSymbols(simpleConstraint: SimpleConstraint): Constraint =
      simpleConstraint.map { sc =>
        Polynomial(
          for {
            term <- sc.v.terms
          } yield replaceThisSymbol(term)
        )
      }

    private def replaceThisSymbol(term: Term) = Term(term.coeff,
      for {
        (v: SymbolType, mult) <- term.variables
      } yield (v.map { sym =>
        if(sym.isType) thisSymbol.get
        else sym
      }, mult)
    )


    private def createErrorMessage(targetSymbol: Symbol, targetBounds: Constraint,
                                   assignee: Tree, assigneeBounds: Constraint)
                                 (context: Context): String = {
      val targetName = targetSymbol.name
      val assigneeName = assignee.symbol match {
        case NoSymbol => assignee.toString()
        case null => assignee.toString()
        case _ => assignee.symbol.name.toString
      }
      s"""
        |Could not assign $assigneeName to $targetName.
        |Unable to prove that:
        |  $assigneeName constrained by (${assigneeBounds.prettyPrint(assigneeName.toString)})
        |is a subtype of
        |  $targetName constrained by (${targetBounds.prettyPrint(targetName.toString)})
      """.stripMargin
    }
  }
}
