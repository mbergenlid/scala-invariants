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
      try {
        assign(expr)
      } catch {
        case CompilationError(error) =>
          reportError(error)
          BoundedType.noBounds
      }
    }

    private def assign(expr: Tree)(implicit context: Context) = {
      if(expressionForType.isDefinedAt(symbol.typeSignature)) {
        val target =
          for(sc <- BoundsFactory.apply(symbol, symbol.typeSignature)) yield replaceThisSymbols(sc)

        val boundExpr = checkBounds(context)(expr)
        val exprConstraints =
          Context.getConstraint(boundExpr.constraint, symbol.typeSignature, context)

//        val fromConstants =
//          Context.substituteConstants(exprConstraints, symbol.typeSignature, context)
        val assignee =
          exprConstraints //&& fromConstants
        if(!(assignee definitelySubsetOf target))
          reportError(Error(expr.pos, createErrorMessage(symbol, target, expr, assignee)(context)))
        boundExpr
      } else {
        val target = BoundsFactory.apply(symbol)
        val boundExpr = checkBounds(context)(expr)
        val exprConstraints = boundExpr.constraint &&
          Context.getPropertyConstraints(symbolChainFromTree(expr), context)

        if(!(exprConstraints definitelySubsetOf target))
          reportError(Error(expr.pos, createErrorMessage(symbol, target, expr, exprConstraints)(context)))

        boundExpr
      }
    }


    private def replaceThisSymbols(simpleConstraint: SimpleConstraint): Constraint =
      simpleConstraint.map { sc =>
        Polynomial(
          for {
            term <- sc.expression.terms
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
        case NoSymbol => assignee.getClass.getSimpleName
        case null => assignee.getClass.getSimpleName
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
