package mbergenlid.tools.boundedintegers


import scala.reflect.api.Universe
import mbergenlid.tools.boundedintegers.{GreaterThanOrEqual => GreaterThanOrEqualAnnotation,
                                         LessThanOrEqual => LessThanOrEqualAnnotation,
                                         Equal => EqualAnnotation}

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

  object BoundsFactory {
    def apply(bounds: Annotation): BoundedInteger = bounds match {
      case a if a.tpe =:= typeOf[GreaterThanOrEqualAnnotation] =>
        BoundedInteger(GreaterThanOrEqual(expr(a.scalaArgs.head)))
      case a if a.tpe =:= typeOf[LessThanOrEqualAnnotation] =>
        BoundedInteger(LessThanOrEqual(expr(a.scalaArgs.head)))
      case a if a.tpe =:= typeOf[EqualAnnotation] =>
        BoundedInteger(Equal(expr(a.scalaArgs.head)))
    }

    private def expr(tree: Tree): Expression = tree match {
      case Literal(Constant(x: Int)) if x != Int.MinValue && x != Int.MaxValue => ConstantValue(x)
      case x: Ident if x.symbol != NoSymbol => SymbolExpression(x.symbol)
    }

    def apply(symbol: Symbol): BoundedInteger = {
      (BoundedInteger.noBounds /: symbol.annotations.collect {
        case a if a.tpe <:< typeOf[BoundedType] => BoundsFactory(a)
      }) (_ && _)
    }

    def apply(tree: Tree): BoundedInteger = {
      tree match {
        case Literal(Constant(value: Int)) =>
          BoundedInteger(Equal(ConstantValue(value)))
        case t if t.symbol != null =>
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
    case _ if bounds != BoundedInteger.noBounds =>
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
