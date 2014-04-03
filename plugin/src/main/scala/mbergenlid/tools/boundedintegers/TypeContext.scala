package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait TypeContext { self: BoundedTypeTrees =>

  def expressionForType: PartialFunction[TypeType, ExpressionFactory[_]]

  class Context(private val symbols: Map[SymbolType, Constraint]) {
    type Operator = (Constraint, Constraint) => Constraint
    def this() = this(Map.empty)
    def apply(symbol: SymbolType) = symbols.get(symbol)
    def get(symbol: SymbolType) = symbols.getOrElse(symbol, NoConstraints)
    def removeSymbolConstraints(symbol: SymbolType) =
      new Context(symbols map { case (k,v) => k -> _removeSymbolConstraints(symbol)(v)})

    private def _removeSymbolConstraints(symbol: SymbolType)(c: Constraint): Constraint = c match {
      case a @ And(left, right) => a.map(_removeSymbolConstraints(symbol))
      case o @ Or(left, right) => o.map(_removeSymbolConstraints(symbol))
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


    def getConstraint(symbol: SymbolType, resultType: TypeType, context: Context): Constraint = {
      val f = expressionForType(resultType)
      val constraint =
        And.combine(createConstraintFromSymbol(symbol), context.get(symbol)).map { sc =>
          f.convertExpression(sc.v)
        }
      for {
        sc <- constraint
      } yield substitute(sc, sc.v.extractSymbols.filterNot(_ == symbol).toList, resultType, context - symbol)

    }

    def getConstraint(start: Constraint, resultType: TypeType, context: Context): Constraint = {
      val f = expressionForType.lift(resultType)
      if(f.isDefined)
        for {
          sc <- start.map(s => f.get.convertExpression(s.v))
        } yield substitute(sc, sc.v.extractSymbols.toList, resultType, context)
      else
        NoConstraints
    }

    private[boundedintegers]
    def substitute( constraint: Constraint,
                    symbols: List[SymbolType],
                    resultType: TypeType,
                    context: Context): Constraint =
      symbols match {
        case symbol :: rest => 
          val b = getConstraint(symbol, resultType, context)
          substitute (
            And.combine(constraint,
              for {
                sc1 <- constraint
                sc2 <- b
                s <- trySubstitute(symbol, sc1, sc2)
              } yield s
            ),
            rest,
            resultType,
            context - symbol
          )
        case Nil => constraint
      }

    protected[boundedintegers] def trySubstitute(
          symbol: SymbolType, base: SimpleConstraint, boundedBy: SimpleConstraint) = {

      for {
        term <- base.v.terms.find(_.variables.contains(symbol))
        f <- if(term.coeff.isLessThanZero)
               createNegativeBoundConstraint(base, boundedBy)
             else
               createBoundConstraint(base, boundedBy)
      } yield {f(base.v.substitute(symbol, boundedBy.v))}


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
        Some(LessThanOrEqual.apply)
      case (LessThanOrEqual(_), GreaterThan(_)) =>
        Some(LessThan.apply)
      case (LessThanOrEqual(_), GreaterThanOrEqual(_)) =>
        Some(LessThanOrEqual.apply)
      case (LessThanOrEqual(_), Equal(_)) =>
        Some(GreaterThanOrEqual.apply)

      case (Equal(_), _) =>
        if(boundedBy.isInstanceOf[Equal])
          Some(Equal.apply)
        else
          createNegativeBoundConstraint(boundedBy, base)
      case _ => None
    }
  }


  class BoundedType(val expression: Option[Expression], val constraint: Constraint) {
    assert(constraint == NoConstraints || tpe != TypeNothing)
    protected def this() = this(None, NoConstraints)
//    def this(tpe: TypeType) = this(NoConstraints)

    def tpe = expression.map(_.tpe).getOrElse(TypeNothing)
    private def assertSameType(other: BoundedType) =
      assert(tpe == TypeNothing || other.tpe == TypeNothing || tpe == other.tpe)

    def convertTo(tpe: TypeType): BoundedType = {
      val f = expressionForType(tpe)
      val newConstraint = constraint.map { sc => f.convertExpression(sc.v) }
      new BoundedType(expression.map(e => f.convertExpression(e)), newConstraint)
    }

    def <:<(other: BoundedType): Boolean =
      constraint.obviouslySubsetOf(other.constraint)

    override def toString = s"BoundedInteger(${constraint.prettyPrint()})"
    override def equals(other: Any) = 
      other.isInstanceOf[BoundedType] &&
      other.asInstanceOf[BoundedType].constraint == this.constraint
  }

  object BoundedType {
    def apply(expression: Expression, constraint: Constraint) = {
      expressionForType.lift(expression.tpe) match {
        case Some(expressionFactory) =>
          new BoundedType(Some(expression), constraint.map { sc =>
            expressionFactory.convertExpression(sc.v)
          })
        case None => BoundedType.noBounds
      }
    }

    def apply(expression: Option[Expression], constraint: Constraint) = {
      new BoundedType(expression, constraint)
    }

    def noBounds = new BoundedType()
  }
}
