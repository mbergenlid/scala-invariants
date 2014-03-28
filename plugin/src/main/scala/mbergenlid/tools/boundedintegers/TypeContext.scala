package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait TypeContext { self: BoundedTypeTrees =>

  def expressionForType: PartialFunction[TypeType, ExpressionFactory[_]]

  class Context(private val symbols: Map[SymbolType, BoundedInteger]) {
    type Operator = (BoundedInteger, BoundedInteger) => BoundedInteger
    def this() = this(Map.empty)
    def apply(symbol: SymbolType) = symbols.get(symbol)
    def get(symbol: SymbolType) = symbols.getOrElse(symbol, new BoundedInteger(symbol.typeSignature))
    def removeSymbolConstraints(symbol: SymbolType) =
      new Context(symbols map { case (k,v) => k -> v.removeSymbolConstraints(symbol)})

    def &&(other: Context) = combineWith(other, _&&_)

    def ||(other: Context) = combineWith(other, _||_)

    def combineWith(other: Context, op: Operator) = {
      val map = (other.symbols /: symbols) { (map, t) =>
        val bounds = other.symbols.getOrElse(t._1, BoundedInteger.noBounds)
        map + (t._1 -> op(bounds, t._2))
      }
      new Context(map)
    }

    def &(s: SymbolType, bounds: BoundedInteger) = {
      val oldBounds = symbols.getOrElse(s, new BoundedInteger(bounds.tpe))
      new Context(symbols + (s -> (oldBounds && bounds)))
    }
    def -(s: SymbolType) = new Context(symbols - s)
      
    def unary_! = new Context(symbols map (kv => kv._1 -> !kv._2))
    def size = symbols.size
    override def toString = symbols.toString()
  } 
  
  def createBound(symbol: SymbolType): BoundedInteger
  object Context {


    def getBoundedInteger(symbol: SymbolType, resultType: TypeType, context: Context): BoundedInteger = {
      val bounds = context(symbol).getOrElse(createBound(symbol)).convertTo(resultType)
      getBoundedInteger(bounds, context - symbol)
    }

    def getBoundedInteger(start: BoundedInteger, context: Context): BoundedInteger = {
      assert(start.tpe != TypeNothing)
      BoundedInteger(
        findTransitiveConstraints(start.constraint, start.tpe, context),
        start.tpe
      )
    }

    /**
     *   _ < x && _ < y
     * upperBound
     *   _ < x && _ < y
     * 
     */
    def findTransitiveConstraints(c: Constraint, resultType: TypeType, context: Context): Constraint =
      c.map {sc: SimpleConstraint =>
        substitute(sc, sc.v.extractSymbols.toList, resultType, context)
      }

    private[boundedintegers]
    def substitute( constraint: Constraint,
                    symbols: List[SymbolType],
                    resultType: TypeType,
                    context: Context): Constraint =
      symbols match {
        case symbol :: rest => 
          val b = getBoundedInteger(symbol, resultType, context).constraint
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
            resultType,
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

    private def findBoundFunction(c: SimpleConstraint): (Constraint => Constraint) = c match {
      case LessThan(v) => _.upperBound
      case GreaterThan(_) => _.lowerBound
      case Equal(_) => con =>
        And(con.lowerBoundInclusive, con.upperBoundInclusive)
      case GreaterThanOrEqual(_) => _.lowerBoundInclusive
      case LessThanOrEqual(_) => _.upperBoundInclusive
    }
  }

  class BoundedInteger(val constraint: Constraint, val tpe: TypeType) {
    assert(constraint == NoConstraints || tpe != TypeNothing)
    private def this() = this(NoConstraints, TypeNothing)
    def this(tpe: TypeType) = this(NoConstraints, tpe)

    private def assertSameType(other: BoundedInteger) =
      assert(tpe == TypeNothing || other.tpe == TypeNothing || tpe == other.tpe)

    def convertTo(tpe: TypeType): BoundedInteger = {
      val f = expressionForType(tpe)
      val newConstraint = constraint.map { sc => f.convertExpression(sc.v) }
      BoundedInteger(newConstraint, tpe)
    }



    def <:<(other: BoundedInteger): Boolean =
      constraint.obviouslySubsetOf(other.constraint)


    def &&(other: BoundedInteger) = {
      assertSameType(other)
      if(constraint == NoConstraints) new BoundedInteger(other.constraint, other.tpe)
      else if(other.constraint == NoConstraints) new BoundedInteger(constraint, tpe)
      else new BoundedInteger(And(constraint, other.constraint), tpe)
    }

    def ||(other: BoundedInteger) = {
      assertSameType(other)
      if(constraint == NoConstraints) new BoundedInteger(other.constraint, other.tpe)
      else if(other.constraint == NoConstraints) new BoundedInteger(constraint, tpe)
      else new BoundedInteger(Or(constraint, other.constraint), tpe)
    }

    /** 
     *  x < y < 10
     *  y < 10 && y > 0
     *
     *  x < y 
     *  y < 10 || y > 100
     */
    def <|(other: BoundedInteger) = {
      assertSameType(other)
      BoundedInteger(other.constraint.upperBound, tpe) && this
    }


    def >|(other: BoundedInteger) = {
      assertSameType(other)
      BoundedInteger(other.constraint.lowerBound, tpe) && this
    }

    def removeSymbolConstraints(symbol: SymbolType): BoundedInteger =
      new BoundedInteger(_removeSymbolConstraints(symbol)(constraint), tpe)

    private def _removeSymbolConstraints(symbol: SymbolType)(c: Constraint): Constraint = c match {
      case a @ And(left, right) => a.map(_removeSymbolConstraints(symbol) _)
      case o @ Or(left, right) => o.map(_removeSymbolConstraints(symbol) _)
      case _ if c.isSymbolConstraint => NoConstraints
      case _ => c
    }

    def unary_! = new BoundedInteger(!constraint, tpe)

    override def toString = s"BoundedInteger(${constraint.prettyPrint()})"
    override def equals(other: Any) = 
      other.isInstanceOf[BoundedInteger] &&
      other.asInstanceOf[BoundedInteger].constraint == this.constraint
  }

  object BoundedInteger {
    def apply(constraint: Constraint, tpe: TypeType) = {
      new BoundedInteger(constraint, tpe)
    }

    def noBounds = new BoundedInteger
  }
}
