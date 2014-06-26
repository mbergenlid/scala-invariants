package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait TypeContext { self: Constraints =>

  def expressionForType: PartialFunction[TypeType, ExpressionFactory[_]]

  class Context(private val symbols: Map[SymbolType, Constraint]) {
    type Operator = (Constraint, Constraint) => Constraint
    def this() = this(Map.empty)
    def apply(symbol: SymbolType) = symbols.get(symbol)
    def get(symbol: SymbolType) = symbols.getOrElse(symbol, NoConstraints)
    def removeSymbolConstraints(symbol: SymbolType) =
      new Context(symbols map { case (k,v) => k -> _removeSymbolConstraints(symbol)(v)})

    private def _removeSymbolConstraints(symbol: SymbolType)(c: Constraint): Constraint = c match {
      case a @ And(_) => a.map(_removeSymbolConstraints(symbol))
      case o @ Or(_) => o.map(_removeSymbolConstraints(symbol))
      case _ if c.isSymbolConstraint => NoConstraints
      case _ => c
    }

    def &&(other: Context) = combineWith(other, _&&_)

    def ||(other: Context) = combineWith(other, _||_)

    def combineWith(other: Context, op: Operator) = {
      val map = (other.symbols /: symbols) { (map, t) =>
        val constraint = other.symbols.getOrElse(t._1, NoConstraints)
        map + (t._1 -> op(constraint, t._2))
      }
      new Context(map)
    }

    def add(s: SymbolType, constraint: Constraint) = {
      val oldConstraint = symbols.getOrElse(s, NoConstraints)
      new Context(symbols + (s -> (oldConstraint && constraint)))
    }
    def -(s: SymbolType) = new Context(symbols - s)
      
    def unary_! = new Context(symbols map (kv => kv._1 -> !kv._2))
    def size = symbols.size
    override def toString = symbols.toString()
  } 
  
  def createConstraintFromSymbol(symbol: SymbolType): Constraint
  object Context {

    def getPropertyConstraints(symbol: SymbolType, context: Context) = {
      val propConstraints = context.symbols.collect {
        case (sym, constraint) if sym.symbols.endsWith(symbol.symbols) =>
          PropertyConstraint(sym.symbols.head, constraint)
      }
      val constraints: Iterable[Constraint] = for {
        prop <- propConstraints
        if expressionForType.lift(prop.symbol.typeSignature).isDefined
      } yield prop.copy(constraint =
          prop.constraint.map { sc =>
            substitute(sc, sc.expression.extractSymbols.toList, prop.symbol.typeSignature, context)
          }
        )

      if(constraints.isEmpty) NoConstraints
      else constraints.reduce(_ && _)
    }

    def getConstraint(symbol: SymbolType, resultType: TypeType, context: Context): Constraint = {
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

    def getConstraint(start: Constraint, resultType: TypeType, context: Context): Constraint = {
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

    def substituteConstants(
      from: Constraint,
      resultType: TypeType,
      context: Context): Constraint = {

        val f = expressionForType(resultType)

        def extractConstant(expr: Expression): ConstantValue =
          expr.terms.find(_.variables.isEmpty).map(_.coeff).getOrElse(Polynomial.Zero.asConstant)

        def fromConstant(ec: ExpressionConstraint) = for {
          boundedBy <- findSymbolConstraints(extractConstant(ec.expression), context, f)
          if boundedBy.expression != ec.expression
          newConstraint <- ec.combine(boundedBy)
        } yield newConstraint(ec.expression.substituteConstant(boundedBy.expression))

        from.map { fromSc: ExpressionConstraint =>
          val l: Iterable[Constraint] = fromConstant(fromSc)
          val c = l.reduceLeftOption(_&&_).getOrElse(NoConstraints)
          c
        }
    }

    private def findSymbolConstraints(
      constant: ConstantValue,
      context: Context,
      f: ExpressionFactory[_]): Iterable[ExpressionConstraint] = {

      val seq = for {
        (symbol, constraint) <- context.symbols
        sc: SimpleConstraint <- constraint
      } yield constraintFromConstant(sc, symbol, constant, f)
      seq.collect {
        case ec:ExpressionConstraint => ec
      }
    }


    private def constraintFromConstant(sc: SimpleConstraint,
                                       boundSymbol: SymbolType,
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

    private[boundedintegers]
    def substitute( constraint: Constraint,
                    symbols: List[SymbolType],
                    resultType: TypeType,
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

    private def getConstraintWitUpperLowerBounds(symbol: SymbolType, resultType: TypeType, context: Context) = {
      val c = getConstraint(symbol, resultType, context) &&
        translatePropertyConstraints(symbol, resultType, context)
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

    private def translatePropertyConstraints(
        symbol: SymbolType,
        resultType: TypeType,
        context: Context): Constraint = {

      val c = context.get(symbol.tail)
      val cList = for {
        prop <- c.propertyConstraints
        if prop.symbol == symbol.head
      } yield {
        prop.constraint
      }
      if(cList.isEmpty) NoConstraints
      else cList.reduce(_&&_)
    }
  }


  class BoundedType(val constraint: Constraint) {
    protected def this() = this(NoConstraints)

    def convertTo(tpe: TypeType): BoundedType = {
      val f = expressionForType(tpe)
      val newConstraint = constraint.map { sc => f.convertExpression(sc.expression) }
      new BoundedType(newConstraint)
    }

    def <:<(other: BoundedType): Boolean =
      constraint.definitelySubsetOf(other.constraint)

    override def toString = s"BoundedInteger(${constraint.prettyPrint()})"
    override def equals(other: Any) = 
      other.isInstanceOf[BoundedType] &&
      other.asInstanceOf[BoundedType].constraint == this.constraint
  }

  object BoundedType {
    def apply(constraint: Constraint, expressionFactory: ExpressionFactory[_]) = {
      new BoundedType(constraint.map { sc =>
        expressionFactory.convertExpression(sc.expression)
      })
    }

    def apply(constraint: Constraint) = {
      new BoundedType(constraint)
    }

    def noBounds = new BoundedType()
  }
}
