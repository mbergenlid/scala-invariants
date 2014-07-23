package mbergenlid.scalainvariants.api

import mbergenlid.scalainvariants.api.constraints.{ExpressionConstraints, Constraints}
import mbergenlid.scalainvariants.api.expressions._

import scala.reflect.api.{Types, Symbols}

trait ApiUniverse extends Expressions
                          with ExpressionParsers
                          with ExpressionFactories
                          with Constraints
                          with ExpressionConstraints
                          with Contexts
                          with ContextLookup
{

  type SymbolType <: Symbols#SymbolApi

  def expressionForType: PartialFunction[Types#TypeApi, ExpressionFactory[_]]

  def createConstraintFromSymbol(symbol: SymbolType): Constraint
}
