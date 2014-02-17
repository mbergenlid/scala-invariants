package mbergenlid.tools.boundedintegers


import scala.reflect.api.Universe
import scala.language.implicitConversions

trait MyUniverse extends BoundedTypeTrees {
  val global: Universe
  import global._

  type BoundedSymbol = global.Symbol
  trait BoundedTypeError {
    def pos: Position
    def message: String
  }
  case class Error(pos: Position, message: String) extends BoundedTypeError
  case class Warning(pos: Position, message: String) extends BoundedTypeError

  class Context(private val symbols: Map[Symbol, BoundedInteger]) {
    type Operator = (BoundedInteger, BoundedInteger) => BoundedInteger
    def this() = this(Map.empty)
    def apply(symbol: Symbol) = symbols.get(symbol)
    def get(symbol: Symbol) = symbols.getOrElse(symbol, BoundedInteger.noBounds)
    def removeSymbolConstraints(symbol: Symbol) =
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

    def &(s: Symbol, bounds: BoundedInteger) = {
      val oldBounds = symbols.getOrElse(s, new BoundedInteger)
      new Context(symbols + (s -> (oldBounds && bounds)))
    }
      
    def unary_! = new Context(symbols map (kv => kv._1 -> !kv._2))
    def size = symbols.size
    override def toString = symbols.toString
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

    def >|(other: BoundedInteger) = this

    def removeSymbolConstraints(symbol: Symbol): BoundedInteger =
      new BoundedInteger(_removeSymbolConstraints(symbol)(constraint))

    private def _removeSymbolConstraints(symbol: Symbol)(c: Constraint): Constraint = c match {
      case a @ And(left, right) => a.map(_removeSymbolConstraints(symbol) _)
      case o @ Or(left, right) => o.map(_removeSymbolConstraints(symbol) _)
      case LessThan(SymbolExpression(s)) => NoConstraints
      case LessThanOrEqual(SymbolExpression(s)) => NoConstraints
      case GreaterThan(SymbolExpression(s)) => NoConstraints
      case GreaterThanOrEqual(SymbolExpression(s)) => NoConstraints
      case Equal(SymbolExpression(s)) => NoConstraints
      case _ => c
    }
    def unary_! = new BoundedInteger(!constraint)

    override def toString = s"BoundedInteger($constraint)"
  }
    /**
     * ----{-----}----
     * ------}-{------
     */

  object BoundedInteger {
    def apply(bounds: Annotation): BoundedInteger = {
      val List(min, max) = bounds.scalaArgs
      val maxOption = extractExpression(max)
      val minOption = extractExpression(min)

      (minOption, maxOption) match {
        case (None, None) => new BoundedInteger
        case (None, Some(x)) => new BoundedInteger(LessThanOrEqual(x))
        case (Some(x), None) => new BoundedInteger(GreaterThanOrEqual(x))
        case (Some(x), Some(y)) => new BoundedInteger(And(
          LessThanOrEqual(y),
          GreaterThanOrEqual(x)
        ))
      }
    }

    private def extractExpression(tree: Tree): Option[Expression] = tree match {
      case Literal(Constant(x: Int)) if(x != Int.MinValue && x != Int.MaxValue) => Some(ConstantValue(x))
      case x: Ident if(x.symbol != NoSymbol) => Some(SymbolExpression(x.symbol))
      case _ => None
    }

    def apply(tree: Tree): BoundedInteger = {
      tree match {
        case Literal(Constant(value: Int)) =>
          BoundedInteger(Equal(ConstantValue(value)))
        case t if(t.symbol != null) =>
          val annotationOption = t.symbol.annotations.find( _.tpe =:= typeOf[Bounded])
          annotationOption match {
            case Some(annotation) => this.apply(annotation)
            case None => noBounds
          }
        case _ => noBounds
      }
    }

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


abstract class BoundedTypeChecker(val global: Universe) extends MyUniverse
                                                with AbstractBoundsValidator
                                                with TypeConstraintValidator
                                                with BooleanExpressionEvaluator {

  import global._
  var errors: List[BoundedTypeError] = Nil

  def reportError(error: BoundedTypeError) {
    errors = error :: errors
  }

  def checkBoundedTypes(tree: Tree): List[BoundedTypeError] = {
    errors = Nil
    checkBounds(new Context())(tree)
    errors.reverse
  }

  def checkBounds(context: Context)(tree: Tree) = {
    if(tree.children.isEmpty) {
      getBoundedIntegerFromContext(tree, context)
    } else tree match {
      case Select(_,_) => getBoundedIntegerFromContext(tree, context)
      case _ => 
        (context /: tree.children) {(c,child) =>
          val bounds = checkBounds(c)(child)
          updateContext(c, child, bounds)
        }
        BoundedInteger.noBounds
    }
  }

  def updateContext(context: Context, tree: Tree, bounds: BoundedInteger): Context = tree match {
    case Assign(_, _) => context.removeSymbolConstraints(tree.symbol)
    case _ if(bounds != BoundedInteger.noBounds) =>
      context && new Context(Map(tree.symbol -> bounds))
    case _ => context      
  }

  private def getBoundedIntegerFromContext(tree: Tree, context: Context) = 
    context(tree.symbol) match {
      case Some(x) => x
      case None => { BoundedInteger(tree) }
    }

}
