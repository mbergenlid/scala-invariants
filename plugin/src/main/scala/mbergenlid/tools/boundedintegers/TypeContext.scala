package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait TypeContext { self: BoundedTypeTrees =>

  def expressionForType: PartialFunction[TypeType, ExpressionFactory[_]]

  class Context(private val symbols: Map[SymbolType, Constraint]) {
    type Operator = (Constraint, Constraint) => Constraint
    def this() = this(Map.empty)
    def apply(symbol: SymbolType) = symbols.get(symbol)
    def get(symbol: SymbolType) = symbols.getOrElse(symbol, new BoundedType(symbol.typeSignature))
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
  
  def createBound(symbol: SymbolType): BoundedType
  object Context {


    def getConstraint(symbol: SymbolType, context: Context): Constraint = {
      val constraint = context(symbol).getOrElse(createBound(symbol).constraint)
      getConstraint(constraint, context)
    }

    def getConstraint(start: Constraint, context: Context): Constraint = {
      findTransitiveConstraints(start, context)
    }

    /**
     *   _ < x && _ < y
     * upperBound
     *   _ < x && _ < y
     * 
     */
    def findTransitiveConstraints(c: Constraint, context: Context): Constraint =
      c.map {sc: SimpleConstraint =>
        substitute(sc, sc.v.extractSymbols.toList, context)
      }

    private[boundedintegers]
    def substitute( constraint: Constraint,
                    symbols: List[SymbolType],
                    context: Context): Constraint =
      symbols match {
        case symbol :: rest => 
          val b = getConstraint(symbol, context)
          substitute (
            And.combine(constraint,
              constraint.newFlatMap { sc1 =>
                b.newFlatMap {sc2 =>
                  createBoundConstraint(sc1, sc2).map {f =>
                    val c = f(sc1.v.substitute(symbol, sc2.v))
                    c
                  }
                }
              }
            ),
            rest,
            context - symbol
          )
        case Nil => constraint
      }

//    for {
//      sc1 <- constraint
//      sc2: SimpleConstraint <- findBoundFunction(sc1)(b)
//    } yield {
//      val boundConstr = createBoundConstraint(sc1, sc2)
//      if(boundConstr.isDefined) boundConstr.get(sc1.v.substitute(symbol, sc2.v))
//      else NoConstraints
//    }

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
  }

  class BoundedType(val constraint: Constraint, val tpe: TypeType) {
    assert(constraint == NoConstraints || tpe != TypeNothing)
    def this() = this(NoConstraints, TypeNothing)
    def this(tpe: TypeType) = this(NoConstraints, tpe)

    private def assertSameType(other: BoundedType) =
      assert(tpe == TypeNothing || other.tpe == TypeNothing || tpe == other.tpe)

    def convertTo(tpe: TypeType): BoundedType = {
      val f = expressionForType(tpe)
      val newConstraint = constraint.map { sc => f.convertExpression(sc.v) }
      BoundedType(newConstraint, tpe)
    }



    def <:<(other: BoundedType): Boolean =
      constraint.obviouslySubsetOf(other.constraint)


    def &&(other: BoundedType) = {
      assertSameType(other)
      if(constraint == NoConstraints) new BoundedType(other.constraint, other.tpe)
      else if(other.constraint == NoConstraints) new BoundedType(constraint, tpe)
      else new BoundedType(And(constraint, other.constraint), tpe)
    }

    def ||(other: BoundedType) = {
      assertSameType(other)
      if(constraint == NoConstraints) new BoundedType(other.constraint, other.tpe)
      else if(other.constraint == NoConstraints) new BoundedType(constraint, tpe)
      else new BoundedType(Or(constraint, other.constraint), tpe)
    }

    /** 
     *  x < y < 10
     *  y < 10 && y > 0
     *
     *  x < y 
     *  y < 10 || y > 100
     */
    def <|(other: BoundedType) = {
      assertSameType(other)
      BoundedType(other.constraint.upperBound, tpe) && this
    }


    def >|(other: BoundedType) = {
      assertSameType(other)
      BoundedType(other.constraint.lowerBound, tpe) && this
    }

    def unary_! = new BoundedType(!constraint, tpe)

    override def toString = s"BoundedInteger(${constraint.prettyPrint()})"
    override def equals(other: Any) = 
      other.isInstanceOf[BoundedType] &&
      other.asInstanceOf[BoundedType].constraint == this.constraint
  }

  object BoundedType {
    def apply(constraint: Constraint, tpe: TypeType) = {
      expressionForType.lift(tpe) match {
        case Some(expressionFactory) =>
          new BoundedType(constraint.map { sc =>
            expressionFactory.convertExpression(sc.v)
          }, expressionFactory.convertedType)
        case None => BoundedType.noBounds
      }

    }

    def noBounds = new BoundedType
  }
}
