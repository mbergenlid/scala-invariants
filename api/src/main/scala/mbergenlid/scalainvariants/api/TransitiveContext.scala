package mbergenlid.scalainvariants.api

import scala.reflect.api.Types
import mbergenlid.scalainvariants.api.expressions.ExpressionFactory
import mbergenlid.scalainvariants.api.constraints.{LessThanOrEqual, GreaterThanOrEqual, NoConstraints, Constraint}

trait TransitiveContext {
  type Type = Types#TypeApi

  import Constraint._

  def expressionForType: PartialFunction[Type, ExpressionFactory[_]]
  def createConstraintFromSymbol(symbol: SymbolChain): Constraint

  def getConstraint(start: Constraint, resultType: Type, context: Context): Constraint = {
    val f = expressionForType.lift(resultType)
    if(f.isDefined)
      for {
        sc <- start.map(s => f.get.convertExpression(s.expression))
      } yield {
        val s = substitute(sc, sc.expression.extractSymbols.toList, resultType, context)
        s
      }
    else
      NoConstraints
  }

  def getConstraint(symbol: SymbolChain, resultType: Type, context: Context): Constraint = {
    val f = expressionForType(resultType)
    val constraint =
      (if(symbol.isStable) createConstraintFromSymbol(symbol) && context.get(symbol)
      else createConstraintFromSymbol(symbol)).map { sc =>
        f.convertExpression(sc.expression)
      }
    for {
      sc <- constraint
    } yield substitute(
      sc,
      sc.expression.extractSymbols.filterNot(_ == symbol).toList,
      resultType,
      context - symbol)

  }

  private def substitute(
    constraint: Constraint,
    symbols: List[SymbolChain],
    resultType: Type,
    context: Context): Constraint =
      symbols match {
        case symbol :: rest =>
          val b = getConstraintWitUpperLowerBounds(symbol, resultType, context)
          val substituted = for {
            sc1 <- constraint
            sc2 <- b
            s <- sc1.substitute(symbol, sc2)
          } yield s

          substitute (
            constraint && substituted,
            rest,
            resultType,
            context - symbol
          )
        case Nil => constraint
    }


  private def getConstraintWitUpperLowerBounds(symbol: SymbolChain, resultType: Type, context: Context) = {
    val c = getConstraint(symbol, resultType, context)
    val f = expressionForType(resultType)
    (
      if(!c.lowerBound.exists(_.expression.isConstant))
        c && GreaterThanOrEqual(f.MinValue)
      else
        c
      ) && (
      if(!c.upperBound.exists(_.expression.isConstant))
        c && LessThanOrEqual(f.MaxValue)
      else
        c
      )
  }
}
