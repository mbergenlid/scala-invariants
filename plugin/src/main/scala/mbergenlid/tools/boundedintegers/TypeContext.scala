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

    def &(s: SymbolType, constraint: Constraint) = {
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
        } yield substitute(sc, sc.expression.extractSymbols.toList, resultType, context)
      else
        NoConstraints
    }

    def substituteConstants(fromConstraint: Constraint, resultType: TypeType, context: Context): Constraint = {
      val f = expressionForType(resultType)
      val c = for {
        fromSc <- fromConstraint
        expr <- fromSc.expression.terms.find(_.variables.isEmpty)
      } yield fromConstant(expr.coeff, context, f)
      c
    }

    private def fromConstant(constant: ConstantValue, context: Context, f: ExpressionFactory[_]) = {
      val seq = for {
        (symbol, constraint) <- context.symbols
        sc: SimpleConstraint <- constraint
      } yield constraintFromConstant(sc, symbol, constant, f)
      val res = (NoConstraints.asInstanceOf[Constraint] /: seq) (_&&_)
      res
    }


    private def constraintFromConstant(sc: SimpleConstraint,
                                       boundSymbol: SymbolType,
                                       constant: ConstantValue,
                                       f: ExpressionFactory[_]): Constraint = sc match {
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
            s <- trySubstitute(symbol, sc1, sc2)
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
      val c = getConstraint(symbol, resultType, context)
//        translatePropertyConstraints(symbol, resultType, context)
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

//    private def translatePropertyConstraints(symbol: SymbolType, resultType: TypeType, context: Context) = {
//      val c = context.get(symbol.tail)
//      val cList = for {
//        prop <- c.propertyConstraints
//        if prop.symbol == symbol.head
//      } yield {
//        prop.constraint
//      }
//      if(cList.isEmpty) NoConstraints
//      else cList.reduce(_&&_)
//    }

    protected[boundedintegers] def trySubstitute(
          symbol: SymbolType, base: ExpressionConstraint, boundedBy: ExpressionConstraint) = {

      for {
        term <- base.expression.terms.find(_.variables.contains(symbol))
        f <- if(term.coeff.isLessThanZero)
               createNegativeBoundConstraint(base, boundedBy)
             else
               createBoundConstraint(base, boundedBy)
      } yield {f(base.expression.substitute(symbol, boundedBy.expression))}


    }

    protected[boundedintegers] def createBoundConstraint(
          base: SimpleConstraint, boundedBy: SimpleConstraint):
          Option[Expression => SimpleConstraint] = (base, boundedBy) match {

      case (LessThan(_), LessThan(_)) =>
        Some(LessThan.apply)
      case (LessThan(_), LessThanOrEqual(_)) => 
        Some(LessThan.apply)
      case (LessThan(_), Equal(_)) => 
        Some(LessThan.apply)
      case (GreaterThan(_), GreaterThan(_)) => 
        Some(GreaterThan.apply)
      case (GreaterThan(_), GreaterThanOrEqual(_)) => 
        Some(GreaterThan.apply)
      case (GreaterThan(_), Equal(_)) => 
        Some(GreaterThan.apply)

      case (GreaterThanOrEqual(_), GreaterThan(_)) =>
          Some(GreaterThan.apply)
      case (GreaterThanOrEqual(_), GreaterThanOrEqual(_)) =>
          Some(GreaterThanOrEqual.apply)
      case (GreaterThanOrEqual(_), Equal(_)) =>
          Some(GreaterThanOrEqual.apply)
      case (LessThanOrEqual(_), LessThan(_)) =>
          Some(LessThan.apply)
      case (LessThanOrEqual(_), LessThanOrEqual(_)) =>
          Some(LessThanOrEqual.apply)
      case (LessThanOrEqual(_), Equal(_)) =>
          Some(LessThanOrEqual.apply)

      case (Equal(_), _) =>
        if(boundedBy.isInstanceOf[Equal])
          Some(Equal.apply)
        else 
          createBoundConstraint(boundedBy, base)
      case _ => None
    }

    protected[boundedintegers] def createNegativeBoundConstraint(
              base: SimpleConstraint, boundedBy: SimpleConstraint):
              Option[Expression => SimpleConstraint] = (base, boundedBy) match {

      case (LessThan(_), GreaterThan(_)) =>
        Some(LessThan.apply)
      case (LessThan(_), GreaterThanOrEqual(_)) =>
        Some(LessThan.apply)
      case (LessThan(_), Equal(_)) =>
        Some(LessThan.apply)
      case (GreaterThan(_), LessThan(_)) =>
        Some(GreaterThan.apply)
      case (GreaterThan(_), LessThanOrEqual(_)) =>
        Some(GreaterThan.apply)
      case (GreaterThan(_), Equal(_)) =>
        Some(GreaterThan.apply)

      case (GreaterThanOrEqual(_), LessThan(_)) =>
        Some(GreaterThan.apply)
      case (GreaterThanOrEqual(_), LessThanOrEqual(_)) =>
        Some(GreaterThanOrEqual.apply)
      case (GreaterThanOrEqual(_), Equal(_)) =>
        Some(GreaterThanOrEqual.apply)
      case (LessThanOrEqual(_), GreaterThan(_)) =>
        Some(LessThan.apply)
      case (LessThanOrEqual(_), GreaterThanOrEqual(_)) =>
        Some(LessThanOrEqual.apply)
      case (LessThanOrEqual(_), Equal(_)) =>
        Some(LessThanOrEqual.apply)

      case (Equal(_), Equal(_)) =>
        Some(Equal.apply)
      case (Equal(_), LessThan(_)) =>
        Some(GreaterThan.apply)
      case (Equal(_), LessThanOrEqual(_)) =>
        Some(GreaterThanOrEqual.apply)
      case (Equal(_), GreaterThan(_)) =>
        Some(LessThan.apply)
      case (Equal(_), GreaterThanOrEqual(_)) =>
        Some(LessThanOrEqual.apply)

      case _ => None
    }
  }


  class BoundedType(val expression: Option[Expression], val constraint: Constraint) {
    protected def this() = this(None, NoConstraints)
//    def this(tpe: TypeType) = this(NoConstraints)

    def tpe = expression.map(_.tpe).getOrElse(TypeNothing)
    private def assertSameType(other: BoundedType) =
      assert(tpe == TypeNothing || other.tpe == TypeNothing || tpe == other.tpe)

    def convertTo(tpe: TypeType): BoundedType = {
      val f = expressionForType(tpe)
      val newConstraint = constraint.map { sc => f.convertExpression(sc.expression) }
      new BoundedType(expression.map(e => f.convertExpression(e)), newConstraint)
    }

    def <:<(other: BoundedType): Boolean =
      constraint.definitelySubsetOf(other.constraint)

    override def toString = s"BoundedInteger(${constraint.prettyPrint()})"
    override def equals(other: Any) = 
      other.isInstanceOf[BoundedType] &&
      other.asInstanceOf[BoundedType].constraint == this.constraint
  }

  object BoundedType {
    def apply(expression: Expression, constraint: Constraint, expressionFactory: ExpressionFactory[_]) = {
      new BoundedType(Some(expression), constraint.map { sc =>
        expressionFactory.convertExpression(sc.expression)
      })
    }

    def apply(expression: Option[Expression], constraint: Constraint) = {
      new BoundedType(expression, constraint)
    }

    def noBounds = new BoundedType()
  }
}
