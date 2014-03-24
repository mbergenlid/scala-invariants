package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe
import mbergenlid.tools.boundedintegers.annotations.{GreaterThanOrEqual => GreaterThanOrEqualAnnotation, LessThan => LessThanAnnotation, LessThanOrEqual => LessThanOrEqualAnnotation, Equal => EqualAnnotation, Bounded, RichNumeric}
import mbergenlid.tools.boundedintegers.annotations.RichNumeric.{LongIsRichNumeric, IntIsRichNumeric}

trait MyUniverse extends BoundedTypeTrees with TypeContext {
  val global: Universe
  import global._

  type SymbolType = global.Symbol
  type TypeType = global.Type

  trait BoundedTypeError {
    def pos: Position
    def message: String
  }
  case class Error(pos: Position, message: String) extends BoundedTypeError
  case class Warning(pos: Position, message: String) extends BoundedTypeError

  lazy val TypeNothing = typeOf[Nothing]
  lazy val IntType = typeOf[Int]
  lazy val IntSymbol = IntType.typeSymbol
  lazy val LongSymbol = typeOf[Long].typeSymbol
  lazy val DoubleSymbol = typeOf[Double].typeSymbol

  object BoundsFactory {

    def apply(bounds: Annotation, tpe: TypeType): BoundedInteger = bounds match {
      case a if a.tpe =:= typeOf[GreaterThanOrEqualAnnotation] =>
        BoundedInteger(GreaterThanOrEqual(expr(a.scalaArgs.head, tpe)), tpe)
      case a if a.tpe =:= typeOf[LessThanOrEqualAnnotation] =>
        BoundedInteger(LessThanOrEqual(expr(a.scalaArgs.head, tpe)), tpe)
      case a if a.tpe =:= typeOf[EqualAnnotation] =>
        BoundedInteger(Equal(expr(a.scalaArgs.head, tpe)), tpe)
      case a if a.tpe =:= typeOf[LessThanAnnotation] =>
        BoundedInteger(LessThan(expr(a.scalaArgs.head, tpe)), tpe)
    }

    private def expr(tree: Tree, resultType: TypeType): Expression = {
      val Apply(_, List(value)) = tree
      expression(value, resultType)
    }

    def convertExpression[From: RichNumeric](from: From, to: Type): Expression = to match {
      case ConstantType(Constant(x: Int)) =>
        Polynom.fromConstant[Int](implicitly[RichNumeric[Int]].fromType(from))
      case TypeRef(_, IntSymbol, Nil) =>
        Polynom.fromConstant[Int](implicitly[RichNumeric[Int]].fromType(from))
      case NullaryMethodType(IntType) =>
        Polynom.fromConstant[Int](implicitly[RichNumeric[Int]].fromType(from))
      case ConstantType(Constant(x: Long)) =>
        Polynom.fromConstant[Long](implicitly[RichNumeric[Long]].fromType(from))
      case TypeRef(_, LongSymbol, Nil) =>
        Polynom.fromConstant[Long](implicitly[RichNumeric[Long]].fromType(from))
      case TypeRef(_, DoubleSymbol, Nil) =>
        Polynom.fromConstant[Double](implicitly[RichNumeric[Double]].fromType(from))
    }

    def symbolExpression(symbol: SymbolType, tpe: Type): Expression = tpe match {
      case TypeRef(_, IntSymbol, Nil) =>
        Polynom.fromSymbol[Int](symbol)
      case TypeRef(_, LongSymbol, Nil) =>
        Polynom.fromSymbol[Long](symbol)
      case TypeRef(_, DoubleSymbol, Nil) =>
        Polynom.fromSymbol[Double](symbol)
    }

    def expression(tree: Tree, tpe: Type): Expression = tree match {
      case Literal(Constant(x: Int)) if x != Int.MinValue && x != Int.MaxValue =>
        expressionForType(tpe).convertConstant(x)
      case Literal(Constant(x: Long)) =>
        convertExpression(x, tpe)
      case Literal(Constant(x: Double)) =>
        expressionForType(tpe).convertConstant(x)
      case x: Ident if x.symbol != NoSymbol =>
        symbolExpression(x.symbol, tpe)
    }

    def apply(symbol: Symbol, tpe: Type): BoundedInteger = {
      (BoundedInteger.noBounds /: symbol.annotations.collect {
        case a if a.tpe <:< typeOf[Bounded] =>
          BoundsFactory(a, tpe)
      }) (_ && _)
    }

    def apply(tree: Tree): BoundedInteger = {
      tree match {
        case Literal(Constant(value: Int)) =>
          BoundedInteger(Equal(Polynom.fromConstant(value)), tree.tpe)
        case t if t.symbol != null =>
          BoundsFactory(t.symbol, t.tpe)
        case _ => BoundedInteger.noBounds
      }
    }


  }

  def createBound(symbol: SymbolType) = BoundsFactory(symbol, symbol.typeSignature)

  def expressionForType: PartialFunction[TypeType, ExpressionFactory[_]] = {
    case ConstantType(Constant(x: Int)) =>
      new ExpressionFactory[Int]
    case TypeRef(_, IntSymbol, Nil) =>
      new ExpressionFactory[Int]
    case TypeRef(_, LongSymbol, Nil) =>
      new ExpressionFactory[Long]
    case TypeRef(_, DoubleSymbol, Nil) =>
      new ExpressionFactory[Double]

    case NullaryMethodType(IntType) =>
      new ExpressionFactory[Int]
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
      case None =>
        BoundsFactory(tree)
    }
    if(tree.symbol != null && tree.symbol != NoSymbol) {
      val constraintOption =
        for(e <- expressionForType.lift(tree.tpe))
        yield Equal(e.fromSymbol(tree.symbol))

      val resultType = if(bounds.tpe == TypeNothing) tree.tpe else bounds.tpe
      BoundedInteger(constraintOption.getOrElse(NoConstraints), resultType) && bounds
    } else {
      bounds //Context.getBoundedInteger(bounds, context - tree.symbol)
    }
  }

}
