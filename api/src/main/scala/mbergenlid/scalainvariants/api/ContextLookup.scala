package mbergenlid.scalainvariants.api

import scala.reflect.api.Types

trait ContextLookup {
  self: ApiUniverse =>

  object TransitiveContext {
    type Type = Types#TypeApi

    import Constraint._

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

    def getConstraint(symbol: SymbolChain[SymbolType], resultType: Type, context: Context): Constraint = {
      val f = expressionForType(resultType)
      val constraint =
        (if(symbol.isStable) createConstraintFromSymbol(symbol.head) && context.get(symbol)
        else createConstraintFromSymbol(symbol.head)).map { sc =>
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

    def substituteConstants(
      from: Constraint,
      resultType: Type,
      context: Context): Constraint = {

      val f = expressionForType(resultType)

      def extractConstant(expr: Expression): ConstantValue =
        expr.terms.find(_.variables.isEmpty).map(_.coeff).getOrElse(Polynomial.Zero.asConstant)

      def fromConstant(ec: ExpressionConstraint) = for {
        boundedBy <- findSymbolConstraints(extractConstant(ec.expression), context, f)
        if boundedBy.expression != ec.expression
        newConstraint <- ec.combine(boundedBy)
      } yield newConstraint(ec.expression.substituteConstant(boundedBy.expression))

      val constants = for {
        fromSc <- from.toList
      } yield {
        val l: Traversable[Constraint] = fromConstant(fromSc)
        val c = l.reduceLeftOption(_&&_).getOrElse(NoConstraints)
        c
      }
      constants.reduceOption(_&&_).getOrElse(NoConstraints)
    }

    private def findSymbolConstraints(
      constant: ConstantValue,
      context: Context,
      f: ExpressionFactory[_]): Traversable[ExpressionConstraint] = {

      val seq = for {
        (symbol, constraint) <- context.symbols
        if symbol.isStable
        sc: SimpleConstraint <- constraint
      } yield constraintFromConstant(sc, symbol, constant, f)
      seq.collect {
        case ec:ExpressionConstraint => ec
      }
    }

    private def constraintFromConstant(
      sc: SimpleConstraint,
      boundSymbol: SymbolChain[SymbolType],
      constant: ConstantValue,
      f: ExpressionFactory[_]): SimpleConstraint = sc match {

      case GreaterThan(v) if v.isConstant =>
        if(f.convertExpression(v) >= f.convertConstant(constant))
          LessThan(f.fromSymbol(boundSymbol))
        else if(f.convertExpression(v).increment == f.convertConstant(constant))
          LessThanOrEqual(f.fromSymbol(boundSymbol))
        else NoConstraints
      case LessThan(v) if v.isConstant =>
        if(f.convertExpression(v) <= f.convertConstant(constant))
          GreaterThan(f.fromSymbol(boundSymbol))
        else if(f.convertExpression(v).decrement == f.convertConstant(constant))
          GreaterThanOrEqual(f.fromSymbol(boundSymbol))
        else NoConstraints
      case GreaterThanOrEqual(v) if v.isConstant =>
        if(f.convertExpression(v) >= f.convertConstant(constant)) LessThan(f.fromSymbol(boundSymbol))
        else NoConstraints
      case LessThanOrEqual(v) if v.isConstant =>
        if(f.convertExpression(v) <= f.convertConstant(constant)) GreaterThan(f.fromSymbol(boundSymbol))
        else NoConstraints
      case Equal(v) if v.isConstant =>
        val constantExpression = f.convertConstant(constant)
        val constrainedExpression = f.convertExpression(v)
        if(constrainedExpression == constantExpression) Equal(f.fromSymbol(boundSymbol))
        else if(constrainedExpression < constantExpression) GreaterThan(f.fromSymbol(boundSymbol))
        else LessThan(f.fromSymbol(boundSymbol))
      case _ => NoConstraints
    }

    private def substitute(
      constraint: Constraint,
      symbols: List[SymbolChain[SymbolType]],
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


    private def getConstraintWitUpperLowerBounds(symbol: SymbolChain[SymbolType], resultType: Type, context: Context) = {
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

}
