package mbergenlid.tools.boundedintegers


import scala.reflect.api.Universe

trait MyUniverse extends BoundedTypeTrees with TypeContext {
  val global: Universe
  import global._

  type BoundedSymbol = global.Symbol
  trait BoundedTypeError {
    def pos: Position
    def message: String
  }
  case class Error(pos: Position, message: String) extends BoundedTypeError
  case class Warning(pos: Position, message: String) extends BoundedTypeError

    /**
     * ----{-----}----
     * ------}-{------
     */

  object BoundsFactory {
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

    def apply(symbol: Symbol): BoundedInteger = {
      val annotationOption = symbol.annotations.find( _.tpe =:= typeOf[Bounded])
      annotationOption match {
        case Some(annotation) => BoundsFactory(annotation)
        case None => BoundedInteger.noBounds
      }
    }

    def apply(tree: Tree): BoundedInteger = {
      tree match {
        case Literal(Constant(value: Int)) =>
          BoundedInteger(Equal(ConstantValue(value)))
        case t if(t.symbol != null) =>
          BoundsFactory(t.symbol)
        case _ => BoundedInteger.noBounds
      }
    }


  }

  def createBound(symbol: BoundedSymbol) = BoundsFactory(symbol)
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

  def checkBounds(context: Context)(tree: Tree): BoundedInteger = {
    if(tree.children.isEmpty) {
      val b = getBoundedIntegerFromContext(tree, context)
      b
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

  private def getBoundedIntegerFromContext(tree: Tree, context: Context) = {
    val bounds = context(tree.symbol) match {
      case Some(x) => x
      case None => { BoundsFactory(tree) }
    }
    Context.getBoundedInteger(bounds, context - tree.symbol)
  }

}
