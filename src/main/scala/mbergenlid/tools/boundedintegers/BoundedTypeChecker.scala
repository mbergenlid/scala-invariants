package mbergenlid.tools.boundedintegers


import scala.reflect.api.Universe
import scala.language.implicitConversions

trait MyUniverse {
  val global: Universe
  import global._

  trait BoundedTypeError {
    def message: String
  }
  case class Error(message: String) extends BoundedTypeError
  case class Warning(message: String) extends BoundedTypeError

  class Context(private val symbols: Map[Symbol, BoundedInteger]) {
    def this() = this(Map.empty)
    def apply(symbol: Symbol) = symbols.get(symbol)
    def ++(other: Context) = new Context(symbols ++ other.symbols)
    def size = symbols.size
    override def toString = symbols.toString
  } 

  case class BoundedInteger(min: Int = Int.MinValue, max: Int = Int.MaxValue) {

    def <:<(other: BoundedInteger): Boolean = {
      min >= other.min && max <= other.max
    }

    def <|(other: BoundedInteger) =
      this.copy(max = Math.min(max, other.max))

    def >|(other: BoundedInteger) =
      new BoundedInteger(Math.max(min, other.min), max)
  }

  object BoundedInteger {
    def apply(bounds: Annotation): BoundedInteger = {
      val List(Literal(Constant(min: Int)), Literal(Constant(max: Int))) = bounds.scalaArgs
      new BoundedInteger(min, max)
    }

    def apply(tree: Tree): BoundedInteger = {
      assert(tree.tpe <:< typeOf[Int])
      tree match {
        case Literal(Constant(value: Int)) => new BoundedInteger(value, value)
        case t =>
          val annotationOption = t.symbol.annotations.find( _.tpe =:= typeOf[Bounded])
          annotationOption match {
            case Some(annotation) => this.apply(annotation)
            case None => new BoundedInteger
          }
      }
    }

    implicit def integerToBounded(x: Int) = BoundedInteger(x, x)
  }
}


class BoundedTypeChecker(val global: Universe) extends MyUniverse with AbstractBoundsValidator {
  import global._

  def checkBoundedTypes(tree: Tree): List[BoundedTypeError] = {
    checkBounds(new Context())(tree)
  }

  def checkBounds(context: Context)(tree: Tree) = {
    tree.children.flatMap(checkBounds(context))
  }

}
