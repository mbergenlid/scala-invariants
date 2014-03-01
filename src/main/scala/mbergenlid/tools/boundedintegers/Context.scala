package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait TypeContext { self: BoundedTypeTrees =>

  class Context(private val symbols: Map[BoundedSymbol, BoundedInteger]) {
    type Operator = (BoundedInteger, BoundedInteger) => BoundedInteger
    def this() = this(Map.empty)
    def apply(symbol: BoundedSymbol) = symbols.get(symbol)
    def get(symbol: BoundedSymbol) = symbols.getOrElse(symbol, BoundedInteger.noBounds)
    def removeSymbolConstraints(symbol: BoundedSymbol) =
      new Context(symbols map { case (k,v) => (k -> v.removeSymbolConstraints(symbol))})

    def &&(other: Context) = combineWith(other, _&&_)

    def ||(other: Context) = combineWith(other, _||_)

    def combineWith(other: Context, op: Operator) = {
      val map = (other.symbols /: symbols) { (map, t) =>
        val bounds = other.symbols.getOrElse(t._1, new BoundedInteger)
        map + (t._1 -> (op(bounds, t._2)))
      }
      new Context(map)
    }

    def &(s: BoundedSymbol, bounds: BoundedInteger) = {
      val oldBounds = symbols.getOrElse(s, new BoundedInteger)
      new Context(symbols + (s -> (oldBounds && bounds)))
    }
    def -(s: BoundedSymbol) = new Context(symbols - s)
      
    def unary_! = new Context(symbols map (kv => kv._1 -> !kv._2))
    def size = symbols.size
    override def toString = symbols.toString
  } 
  
  def createBound(symbol: BoundedSymbol): BoundedInteger
  object Context {


    def getBoundedInteger(symbol: BoundedSymbol, context: Context): BoundedInteger = {
      val bounds = context(symbol).getOrElse(createBound(symbol))
      getBoundedInteger(bounds, context - symbol)
    }

    def getBoundedInteger(start: BoundedInteger, context: Context): BoundedInteger = {
      BoundedInteger(
        findTransitiveConstraints(start.constraint, context)
      )
    }

    def findTransitiveConstraints(c: Constraint, context: Context): Constraint =
      for {
        sc <- c
        sym <- sc.v.extractSymbols 
        symConstraint <- findBoundFunction(sc)(getBoundedInteger(sym, context).constraint)
      } yield
        createBoundConstraint(sc, symConstraint).apply(
          sc.v.substitute(sym, symConstraint.v)
        )
    
    private def createBoundConstraint(
          base: SimpleConstraint, boundedBy: SimpleConstraint):
          (Expression => SimpleConstraint) = base match {

      case LessThan(v) => 
        LessThan.apply _
      case GreaterThan(v) => 
        GreaterThan.apply _
      case Equal(v) =>
        if(boundedBy.isInstanceOf[Equal])
          Equal.apply _
        else 
          createBoundConstraint(boundedBy, base)
      case GreaterThanOrEqual(v) =>
        GreaterThan.apply _
      case LessThanOrEqual(v) =>
        LessThanOrEqual.apply _
    }

    private def findBoundFunction(c: SimpleConstraint): (Constraint => Constraint) = c match {
      case LessThan(v) => _.upperBound
      case GreaterThan(_) => _.lowerBound
      case Equal(_) => { con => 
        And(con.lowerBound, con.upperBound)
      }
      case GreaterThanOrEqual(_) => _.lowerBound
      case LessThanOrEqual(_) => _.upperBound
    }
  }

  class BoundedInteger(val constraint: Constraint) {
    import BoundedInteger._

    def this() = this(NoConstraints)

    def <:<(other: BoundedInteger): Boolean =
      constraint.obviouslySubsetOf(other.constraint)


    def &&(other: BoundedInteger) = constraint match {
      case NoConstraints => new BoundedInteger(other.constraint)
      case c => new BoundedInteger(And(constraint, other.constraint))
    }
      
    def ||(other: BoundedInteger) = constraint match {
      case NoConstraints => new BoundedInteger(other.constraint)
      case c => new BoundedInteger(Or(constraint, other.constraint))
    }

    /** 
     *  x < y < 10
     *  y < 10 && y > 0
     *
     *  x < y 
     *  y < 10 || y > 100
     */
    def <|(other: BoundedInteger) =
      BoundedInteger(other.constraint.upperBound) && this

    def >|(other: BoundedInteger) =
      BoundedInteger(other.constraint.lowerBound) && this

    def removeSymbolConstraints(symbol: BoundedSymbol): BoundedInteger =
      new BoundedInteger(_removeSymbolConstraints(symbol)(constraint))

    private def _removeSymbolConstraints(symbol: BoundedSymbol)(c: Constraint): Constraint = c match {
      case a @ And(left, right) => a.map(_removeSymbolConstraints(symbol) _)
      case o @ Or(left, right) => o.map(_removeSymbolConstraints(symbol) _)
      case _ if(c.isSymbolConstraint) => NoConstraints
      case _ => c
    }

    def unary_! = new BoundedInteger(!constraint)

    override def toString = s"BoundedInteger(${constraint.prettyPrint()})"
    override def equals(other: Any) = 
      other.isInstanceOf[BoundedInteger] &&
      other.asInstanceOf[BoundedInteger].constraint == this.constraint
  }

  object BoundedInteger {

    def apply(constraint: Constraint) = new BoundedInteger(constraint)
    def apply(min: Int, max: Int) = (min, max) match {
      case (Int.MinValue, Int.MaxValue) => new BoundedInteger()
      case (Int.MinValue, _) => new BoundedInteger(LessThanOrEqual(ConstantValue(max)))
      case (_, Int.MaxValue) => new BoundedInteger(GreaterThanOrEqual(ConstantValue(min)))
      case _ => new BoundedInteger(And(
        LessThanOrEqual(ConstantValue(max)),
        GreaterThanOrEqual(ConstantValue(min))
      ))
    }

    implicit def integerToBounded(x: Int) = BoundedInteger(Equal(ConstantValue(x)))

    val noBounds = new BoundedInteger
  }
}
