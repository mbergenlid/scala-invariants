package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe
import mbergenlid.tools.boundedintegers.annotations.{GreaterThanOrEqual => GreaterThanOrEqualAnnotation, LessThan => LessThanAnnotation, LessThanOrEqual => LessThanOrEqualAnnotation, Equal => EqualAnnotation, Bounded, RichNumeric}
import mbergenlid.tools.boundedintegers.annotations.RichNumeric.{LongIsRichNumeric, IntIsRichNumeric}

trait MyUniverse extends BoundedTypeTrees with TypeContext {
  val global: Universe
  import global._

  type SymbolType = global.Symbol

  trait BoundedTypeError {
    def pos: Position
    def message: String
  }
  case class Error(pos: Position, message: String) extends BoundedTypeError
  case class Warning(pos: Position, message: String) extends BoundedTypeError

  lazy val TypeNothing = typeOf[Nothing]
  lazy val IntType: TypeType = typeOf[Int]
  lazy val LongType = typeOf[Long]
  lazy val DoubleType = typeOf[Double]
  lazy val IntSymbol = IntType.typeSymbol
  lazy val LongSymbol = LongType.typeSymbol
  lazy val DoubleSymbol = DoubleType.typeSymbol

  object BoundsFactory {

    private def constraint(bounds: Annotation, tpe: TypeType): Constraint = bounds match {
      case a if a.tpe =:= typeOf[GreaterThanOrEqualAnnotation] =>
        GreaterThanOrEqual(expr(a.scalaArgs.head, tpe))
      case a if a.tpe =:= typeOf[LessThanOrEqualAnnotation] =>
        LessThanOrEqual(expr(a.scalaArgs.head, tpe))
      case a if a.tpe =:= typeOf[EqualAnnotation] =>
        Equal(expr(a.scalaArgs.head, tpe))
      case a if a.tpe =:= typeOf[LessThanAnnotation] =>
        LessThan(expr(a.scalaArgs.head, tpe))
    }

    private def expr(tree: Tree, resultType: TypeType): Expression = {
      val Apply(_, List(value)) = tree
      expression(value, resultType)
    }

    def convertExpression[From: RichNumeric](from: From, to: TypeType): Expression = to match {
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

    def symbolExpression(symbol: SymbolType, tpe: TypeType): Expression = tpe match {
      case TypeRef(_, IntSymbol, Nil) =>
        Polynom.fromSymbol[Int](symbol)
      case TypeRef(_, LongSymbol, Nil) =>
        Polynom.fromSymbol[Long](symbol)
      case TypeRef(_, DoubleSymbol, Nil) =>
        Polynom.fromSymbol[Double](symbol)
    }

    def expression(tree: Tree, tpe: TypeType): Expression = tree match {
      case Literal(Constant(x: Int)) if x != Int.MinValue && x != Int.MaxValue =>
        expressionForType(tpe).convertConstant(x)
      case Literal(Constant(x: Long)) =>
        convertExpression(x, tpe)
      case Literal(Constant(x: Double)) =>
        expressionForType(tpe).convertConstant(x)
      case x: Ident if x.symbol != NoSymbol =>
        symbolExpression(x.symbol, tpe)
    }

    def apply(symbol: Symbol, tpe: TypeType): BoundedType = {
      val c =
        (NoConstraints.asInstanceOf[Constraint] /: symbol.annotations.collect {
          case a if a.tpe <:< typeOf[Bounded] =>
            BoundsFactory.constraint(a, tpe)
        }) (_ && _)
      BoundedType(c, tpe)
    }

    def apply(tree: Tree): BoundedType = {
      if(expressionForType.isDefinedAt(tree.tpe)) {
        val f = expressionForType(tree.tpe)
        if(tree.symbol != null && tree.symbol != NoSymbol) {
          BoundsFactory(tree.symbol, f.convertedType)
        } else {
          BoundedType(Equal(expression(tree, tree.tpe)), f.convertedType)
        }
      } else {
        BoundedType.noBounds
      }
    }


  }

  def createBound(symbol: SymbolType) = BoundsFactory(symbol, symbol.typeSignature)

  def expressionForType: PartialFunction[TypeType, ExpressionFactory[_]] = {
    case ConstantType(Constant(x: Int)) =>
      new ExpressionFactory[Int](IntType)
    case ConstantType(Constant(x: Double)) =>
      new ExpressionFactory[Double](DoubleType)
    case TypeRef(_, IntSymbol, Nil) =>
      new ExpressionFactory[Int](IntType)
    case TypeRef(_, LongSymbol, Nil) =>
      new ExpressionFactory[Long](LongType)
    case TypeRef(_, DoubleSymbol, Nil) =>
      new ExpressionFactory[Double](DoubleType)

    case NullaryMethodType(IntType) =>
      new ExpressionFactory[Int](IntType)

    case s@SingleType(_, _) if s <:< IntType.asInstanceOf[Type] =>
      new ExpressionFactory[Int](IntType)
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

  def checkBounds(context: Context)(tree: Tree): BoundedType = {
    if(tree.children.isEmpty) {
      val b = getBoundedIntegerFromContext(tree, context)
      b
    } else tree match {
      case Select(_,_) => getBoundedIntegerFromContext(tree, context)
      case _ => 
        (context /: tree.children) {(c,child) =>
          val bounds = checkBounds(c)(child)
          updateContext(c, child, bounds.constraint)
        }
        BoundedType.noBounds
    }
  }

  def updateContext(context: Context, tree: Tree, constraint: Constraint): Context = tree match {
    case Assign(_, _) => context.removeSymbolConstraints(tree.symbol)
    case _ if constraint != NoConstraints =>
      context && new Context(Map(tree.symbol -> constraint))
    case _ => context      
  }

  private def getBoundedIntegerFromContext(tree: Tree, context: Context): BoundedType = {
    if(expressionForType.isDefinedAt(tree.tpe)) {
      val bounds: Constraint = context(tree.symbol) match {
        case Some(x) => x
        case None =>
          BoundsFactory(tree).constraint
      }
      if(tree.symbol != null && tree.symbol != NoSymbol) {
        val constraintOption =
          for(e <- expressionForType.lift(tree.tpe))
          yield Equal(e.fromSymbol(tree.symbol))

        BoundedType(constraintOption.getOrElse(NoConstraints) &&
          Context.getConstraint(bounds, context - tree.symbol), tree.tpe)
      } else {
        BoundedType(Context.getConstraint(bounds, context - tree.symbol), tree.tpe)
      }
    } else {
      BoundedType.noBounds
    }

  }

}
