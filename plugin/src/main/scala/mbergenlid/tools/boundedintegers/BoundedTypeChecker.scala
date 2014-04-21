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
        GreaterThanOrEqual(annotationExpression(a.scalaArgs.head, tpe))
      case a if a.tpe =:= typeOf[LessThanOrEqualAnnotation] =>
        LessThanOrEqual(annotationExpression(a.scalaArgs.head, tpe))
      case a if a.tpe =:= typeOf[EqualAnnotation] =>
        Equal(annotationExpression(a.scalaArgs.head, tpe))
      case a if a.tpe =:= typeOf[LessThanAnnotation] =>
        LessThan(annotationExpression(a.scalaArgs.head, tpe))
    }

    private def annotationExpression(tree: Tree, resultType: TypeType): Expression = {
      val Apply(_, List(value)) = tree
      expression(value, resultType)
    }

    def expression(tree: Tree, tpe: TypeType): Expression = tree match {
      case Literal(Constant(x: Int)) if x != Int.MinValue && x != Int.MaxValue =>
        expressionForType(tpe).convertConstant(x)
      case Literal(Constant(x: Long)) =>
        expressionForType(tpe).convertConstant(x)
      case Literal(Constant(x: Double)) =>
        expressionForType(tpe).convertConstant(x)
      case Literal(Constant(s: String)) =>
        expressionForType(tpe).fromParameter(s)
      case x if x.symbol != NoSymbol =>
        expressionForType(tpe).fromSymbol(x.symbol)
    }

    def apply(symbol: Symbol, tpe: TypeType): Constraint = {
      val annotadedConstraints = (NoConstraints.asInstanceOf[Constraint] /: symbol.annotations.collect {
        case a if a.tpe <:< typeOf[Bounded] =>
          BoundsFactory.constraint(a, tpe)
      }) (_ && _)
      if(annotadedConstraints == NoConstraints) {
        annotadedConstraints
      } else {
        val f = expressionForType(tpe)
        (
          if(annotadedConstraints.lowerBound != NoConstraints &&
              !annotadedConstraints.lowerBound.isInstanceOf[Equal])
            annotadedConstraints && LessThanOrEqual(f.MaxValue)
          else
            annotadedConstraints
          ) && (
          if(annotadedConstraints.upperBound != NoConstraints &&
              !annotadedConstraints.upperBound.isInstanceOf[Equal])
            annotadedConstraints && GreaterThanOrEqual(f.MinValue)
          else
            annotadedConstraints
          )
      }
    }

    def apply(tree: Tree): BoundedType = {
      if(expressionForType.isDefinedAt(tree.tpe)) {
        val f = expressionForType(tree.tpe)
        val exp = f.convertExpression(expression(tree, tree.tpe))
        if(tree.symbol != null && tree.symbol != NoSymbol) {
          BoundedType(exp, Equal(exp) && BoundsFactory(tree.symbol, f.convertedType))
        } else {
          BoundedType(exp, Equal(exp))
        }
      } else {
        BoundedType.noBounds
      }
    }


  }

  def createConstraintFromSymbol(symbol: SymbolType) = BoundsFactory(symbol, symbol.typeSignature)

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
    case MethodType(params, IntType) =>
      new MethodExpressionFactory[Int](IntType, params)
    case MethodType(params, DoubleType) =>
      new MethodExpressionFactory[Double](DoubleType, params)

    case s@SingleType(_, _) if s <:< IntType.asInstanceOf[Type] =>
      new ExpressionFactory[Int](IntType)
  }

//  case class Scope(expr: Tree) extends Tree {
//
//  }
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

  def checkBounds(context: Context)(tree: Tree): this.BoundedType = {
    def traverseChildren(children: List[Tree]) = {
      (context /: children) {(c,child) =>
        val bounds = checkBounds(c)(child)
        updateContext(c, child, bounds.constraint)
      }
    }
    if(tree.children.isEmpty) {
      val b = BoundsFactory(tree)
      b
    } else tree match {
      case Select(_,_) => BoundsFactory(tree)
      case Block(body, res) =>
        val newContext = traverseChildren(body)
        val bounds = checkBounds(newContext)(res)
        val blockConstraint = Context.getConstraint(bounds.constraint, res.tpe, newContext)
        BoundedType(bounds.expression, blockConstraint)
      case _ => 
        traverseChildren(tree.children)
        BoundedType.noBounds
    }
  }

  def updateContext(context: Context, tree: Tree, constraint: Constraint): Context = tree match {
    case Assign(_, _) => context.removeSymbolConstraints(tree.symbol)
    case _ if constraint != NoConstraints =>
      context && new Context(Map(tree.symbol -> constraint))
    case _ => context      
  }

//  private def getBoundedIntegerFromContext(tree: Tree, context: Context): BoundedType = {
//    if(expressionForType.isDefinedAt(tree.tpe)) {
//      val bounds: Constraint = context(tree.symbol) match {
//        case Some(x) => x
//        case None =>
//          BoundsFactory(tree).constraint
//      }
//      if(tree.symbol != null && tree.symbol != NoSymbol) {
//        val constraintOption =
//          for(e <- expressionForType.lift(tree.tpe))
//          yield Equal(e.fromSymbol(tree.symbol))
//
//        BoundedType(constraintOption.getOrElse(NoConstraints) &&
//          Context.getConstraint(bounds, context - tree.symbol), tree.tpe)
//      } else {
//        BoundedType(Context.getConstraint(bounds, context - tree.symbol), tree.tpe)
//      }
//    } else {
//      BoundedType.noBounds
//    }

//  }

}
