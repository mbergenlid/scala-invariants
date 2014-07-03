package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait TypeConstraintValidator extends AbstractBoundsValidator {
  self: MyUniverse with TypeBoundFactories =>

  import global._

  implicit def symbol2ConstrainedSymbol(symbol: RealSymbolType) =
    new ConstrainedSymbol(SymbolChain(symbol))

  implicit def tree2ConstrainedSymbol(tree: Tree) =
    new ConstrainedSymbol(symbolChainFromTree(tree))

  class ConstrainedSymbol(symbolChain: SymbolType, thisSymbol: Option[Symbol] = None) {
    def this(symbol: SymbolType, thisSymbol: Symbol) = this(symbol, Some(thisSymbol))

    def withThisSymbol(withThisSymbol: Symbol) = new ConstrainedSymbol(symbolChain, withThisSymbol)

    def tryAssign(expr: Tree, boundExpr: BoundedType)(implicit context: Context): BoundedType = {
      try {
        assign(expr, boundExpr)
      } catch {
        case CompilationError(error) =>
          reportError(error)
          BoundedType.noBounds
      }
    }
    def tryAssign(expr: Tree)(implicit context: Context): BoundedType = {
      try {
        assign(expr, checkBounds(context)(expr))
      } catch {
        case CompilationError(error) =>
          reportError(error)
          BoundedType.noBounds
      }
    }

    private def assign(expr: Tree, boundExpr: BoundedType)(implicit context: Context) = {
      val symbol = symbolChain.head
      if(expressionForType.isDefinedAt(symbol.typeSignature)) {
        val target =
          for(sc <- BoundsFactory.fromSymbolChain(symbolChain)) yield replaceThisSymbols(sc)

        val exprConstraints =
          Context.getConstraint(boundExpr.constraint, symbol.typeSignature, context)

        val fromConstants =
          Context.substituteConstants(exprConstraints, symbol.typeSignature, context)

        val fromAnnotatedConstants =
          Context.substituteConstants(exprConstraints, symbol.typeSignature, extractSymbols(target))
        val assignee =
          exprConstraints && fromConstants && fromAnnotatedConstants

        if(!(assignee definitelySubsetOf target))
          reportError(Error(expr.pos, createErrorMessage(symbol, target, expr, assignee)(context)))
        boundExpr
      } else {

        val target = BoundsFactory.propertyConstraints(symbolChain)
        val exprConstraints = boundExpr.constraint &&
          Context.getPropertyConstraints(symbolChainFromTree(expr), context)

        if(!(exprConstraints definitelySubsetOf target))
          reportError(Error(expr.pos, createErrorMessage(symbol, target, expr, exprConstraints)(context)))

        boundExpr
      }
    }

    private def extractSymbols(constraint: Constraint): Context = {
      val map = for {
        ec <- constraint.toList
        symbol <- ec.expression.extractSymbols
      } yield {
        (symbol, BoundsFactory.fromSymbolChain(symbol))
      }
      new Context((Map.empty[SymbolType, Constraint] /: map) {(m, t) =>
        m + (t._1 -> (m.getOrElse(t._1, NoConstraints) && t._2))
      })
    }

    private def replaceThisSymbols(simpleConstraint: SimpleConstraint): Constraint =
      simpleConstraint.map { sc =>
        sc.expression.map { term =>
          replaceThisSymbol(term)
        }
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
