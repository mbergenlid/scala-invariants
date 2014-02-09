package mbergenlid.tools.boundedintegers


import scala.reflect.api.Universe
import scala.language.implicitConversions

trait MyUniverse extends BoundedTypeTrees {
  val global: Universe
  import global._

  type BoundedSymbol = global.Symbol
  trait BoundedTypeError {
    def message: String
  }
  case class Error(message: String) extends BoundedTypeError
  case class Warning(message: String) extends BoundedTypeError

  class Context(private val symbols: Map[Symbol, BoundedInteger]) {
    type Operator = (BoundedInteger, BoundedInteger) => BoundedInteger
    def this() = this(Map.empty)
    def apply(symbol: Symbol) = symbols.get(symbol)
    def &&(other: Context) = combineWith(other, _&&_)

    def ||(other: Context) = combineWith(other, _||_)

    def combineWith(other: Context, op: Operator) = {
      val map = (other.symbols /: symbols) { (map, t) =>
        val bounds = other.symbols.getOrElse(t._1, new BoundedInteger)
        map + (t._1 -> (op(bounds, t._2)))
      }
      new Context(map)
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
      assert(tree.tpe <:< typeOf[Int])
      tree match {
        case Literal(Constant(value: Int)) => BoundedInteger(Equal(ConstantValue(value)))
        case t =>
          val annotationOption = t.symbol.annotations.find( _.tpe =:= typeOf[Bounded])
          annotationOption match {
            case Some(annotation) => this.apply(annotation)
            case None => new BoundedInteger
          }
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

  }
}


class BoundedTypeChecker(val global: Universe) extends MyUniverse
                                                with AbstractBoundsValidator
                                                with BooleanExpressionEvaluator {

  import global._

  def checkBoundedTypes(tree: Tree): List[BoundedTypeError] = {
    checkBounds(new Context())(tree)
  }

  def checkBounds(context: Context)(tree: Tree) = {
    tree.children.flatMap(checkBounds(context))
  }

}
