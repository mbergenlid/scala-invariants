package mbergenlid.scalainvariants.plugin

import mbergenlid.scalainvariants.api.SymbolChain

import scala.language.implicitConversions

trait TypeConstraintValidator extends AbstractBoundsValidator {
  self: MyUniverse =>

  import global._

  import Constraint._

  implicit def symbol2ConstrainedSymbol(symbol: SymbolType) =
    new ConstrainedSymbol(SymbolChain[SymbolType](List(symbol)))

  implicit def tree2ConstrainedSymbol(tree: Tree) =
    new ConstrainedSymbol(symbolChainFromTree(tree))

  class ConstrainedSymbol(symbolChain: SymbolChain[SymbolType], thisSymbol: Option[SymbolType] = None) {
    def this(symbol: SymbolChain[SymbolType], thisSymbol: SymbolType) = this(symbol, Some(thisSymbol))

    def withThisSymbol(withThisSymbol: SymbolType) = new ConstrainedSymbol(symbolChain, withThisSymbol)

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

        val targetSymbols: Set[SymbolChain[SymbolType]] = target.toSet[ExpressionConstraint].flatMap(ec => ec.expression.extractSymbols)

        val exprConstraints: Constraint =
          TransitiveContext.getConstraint(boundExpr.constraint, context)

        val fromConstants =
          TransitiveContext.substituteConstants(exprConstraints, targetSymbols, symbol.typeSignature, context)

        val fromAnnotatedConstants =
          TransitiveContext.substituteConstants(exprConstraints, targetSymbols, symbol.typeSignature, extractSymbols(target))

        val assignee =
          exprConstraints && fromConstants && fromAnnotatedConstants

        if(!(assignee definitelySubsetOf target))
          reportError(Error(expr.pos, createErrorMessage(symbol, target, expr, assignee)(context)))
        boundExpr
      } else {

        val target = BoundsFactory.fromSymbolChain(symbolChain)
        val exprConstraints = boundExpr.constraint &&
          TransitiveContext.getConstraint(context.get(symbolChainFromTree(expr)), context)

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

      map.foldLeft[Context](EmptyContext)(_&&_)
    }

    private def replaceThisSymbols(simpleConstraint: SimpleConstraint): Constraint =
      simpleConstraint.map { sc =>
        sc.expression.map { term =>
          replaceThisSymbol(term)
        }
      }

    private def replaceThisSymbol(term: Term): Term = Term(term.coeff,
      for {
        (v: SymbolChain[SymbolType], mult) <- term.variables
      } yield (v.map { sym =>
        if(sym.isType) thisSymbol.get
        else sym
      }, mult)
    )


    private def createErrorMessage(targetSymbol: SymbolType, targetBounds: Constraint,
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
