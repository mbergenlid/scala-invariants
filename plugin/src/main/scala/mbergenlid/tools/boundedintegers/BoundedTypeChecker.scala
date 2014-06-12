package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe
import mbergenlid.tools.boundedintegers.annotations.{
  GreaterThanOrEqual => GreaterThanOrEqualAnnotation,
  LessThan => LessThanAnnotation,
  LessThanOrEqual => LessThanOrEqualAnnotation,
  Equal => EqualAnnotation,
  Bounded,
  GreaterThan => GreaterThanAnnotation,
  Property => PropertyAnnotation}
import mbergenlid.tools.boundedintegers.annotations.RichNumeric.{LongIsRichNumeric, IntIsRichNumeric}
import mbergenlid.tools.boundedintegers.facades.{TypeFacades, StringFacade}
import scala.reflect.runtime.universe

trait MyUniverse extends Constraints with TypeContext with TypeFacades {
  val global: Universe
  import global._

  type RealSymbolType = global.Symbol

  trait BoundedTypeError {
    def pos: Position
    def message: String
  }
  case class Error(pos: Position, message: String) extends BoundedTypeError
  case class Warning(pos: Position, message: String) extends BoundedTypeError
  case class CompilationError(what: Error) extends Exception


  lazy val TypeNothing = typeOf[Nothing]
  lazy val IntType = typeOf[Int]
  lazy val LongType = typeOf[Long]
  lazy val DoubleType = typeOf[Double]
  lazy val IntSymbol = IntType.typeSymbol
  lazy val LongSymbol = LongType.typeSymbol
  lazy val DoubleSymbol = DoubleType.typeSymbol
  lazy val GreaterThanOrEqualType = typeOf[GreaterThanOrEqualAnnotation]

  object GreaterThanOrEqualExtractor {
    def unapply(tpe: Type): Boolean = {
      tpe =:= GreaterThanOrEqualType
    }
  }

  object BoundsFactory {

    private def constraint(bounds: Annotation, tpe: TypeType): ExpressionConstraint =
      constraint(bounds.tpe, bounds.scalaArgs, tpe)

    private def constraint(
      boundType: Type,
      args: List[Tree],
      resultType: TypeType): ExpressionConstraint = boundType match {
        case GreaterThanOrEqualExtractor() =>
          GreaterThanOrEqual(annotationExpression(args.head, resultType))
        case a if a == typeOf[GreaterThanOrEqualAnnotation] =>
          GreaterThanOrEqual(annotationExpression(args.head, resultType))
        case a if a =:= typeOf[LessThanOrEqualAnnotation] =>
          LessThanOrEqual(annotationExpression(args.head, resultType))
        case a if a =:= typeOf[EqualAnnotation] =>
          Equal(annotationExpression(args.head, resultType))
        case a if a =:= typeOf[LessThanAnnotation] =>
          LessThan(annotationExpression(args.head, resultType))
        case a if a <:< typeOf[GreaterThanAnnotation] =>
          GreaterThan(annotationExpression(args.head, resultType))
    }

    private def annotationExpression(tree: Tree, resultType: TypeType): Expression = {
      val Apply(_, List(value)) = tree
      expression(value, resultType)
    }

    def expression(tree: Tree, tpe: TypeType): Expression = tree match {
      case Literal(Constant(x: Int)) =>
        expressionForType(tpe).convertConstant(x)
      case Literal(Constant(x: Long)) =>
        expressionForType(tpe).convertConstant(x)
      case Literal(Constant(x: Double)) =>
        expressionForType(tpe).convertConstant(x)
      case Literal(Constant(s: String)) =>
        expressionForType(tpe).fromParameter(s)
      case x if x.symbol != NoSymbol =>
        expressionForType(tpe).fromSymbol(symbolChainFromTree(x))
    }

    private def annotatedConstraints(symbol: RealSymbolType, tpe: TypeType): List[ExpressionConstraint] = {
      val annotations = symbol.annotations ++ (
        if(symbol.isMethod && symbol.asMethod.isGetter) symbol.asMethod.accessed.annotations
        else Nil
      )
      annotations.collect {
        case a if a.tpe <:< typeOf[Bounded] =>
          BoundsFactory.constraint(a, tpe)
      }

    }


    private def ensureLowerAndUpperBounds(constraints: Constraint, tpe: TypeType): Constraint = {
      if (constraints == NoConstraints) {
        constraints
      } else {
        val f = expressionForType(tpe)
        val lowerBound = constraints.lowerBound
        val upperBound = constraints.upperBound
        (
          if (lowerBound != NoConstraints &&
            !lowerBound.isInstanceOf[Equal] &&
            !upperBound.exists(_.expression.isConstant))
            constraints && LessThanOrEqual(f.MaxValue)
          else
            constraints
          ) && (
          if (upperBound != NoConstraints &&
            !upperBound.isInstanceOf[Equal] &&
            !lowerBound.exists(_.expression.isConstant))
            constraints && GreaterThanOrEqual(f.MinValue)
          else
            constraints
          )
      }
    }

    def propertyConstraints(symbolChain: SymbolChain): Constraint = {
      val symbol = symbolChain.head
      (NoConstraints.asInstanceOf[Constraint] /: symbol.annotations.collect {
        case a if a.tpe <:< typeOf[PropertyAnnotation] =>
          val (t@Literal(Constant(prop: String))) :: annotations = a.scalaArgs
          val symbolType = symbol match {
            case m: MethodSymbol => m.returnType
            case _ => symbol.typeSignature
          }
          val typeFacade: Type = findFacadeForType(symbolType)
          val memberSymbol = typeFacade.member(newTermName(prop))

          if(memberSymbol == NoSymbol)
            throw new CompilationError(
              Error(t.pos, s"Can not find property $prop in type ${symbol.typeSignature}"))

          if(!isStable(memberSymbol))
            throw new CompilationError(
              Error(t.pos, s"Can not be bound to property $prop in type ${symbol.typeSignature} as " +
                s"it is not stable."))

          if(annotations.isEmpty)
            throw new CompilationError(
              Error(t.pos, "@Property requires at least one constraint"))

          val tpe = memberSymbol match {
            case m: MethodSymbol => m.returnType
            case _ => memberSymbol.typeSignature
          }
//          if(!expressionForType.isDefinedAt(tpe))
//            throw new CompilationError(
//              Error(t.pos, s"Member $memberSymbol in $typeFacade is of unsupported type ${memberSymbol.typeSignature}"))

          val propConstraints: List[Constraint] = for {
            method@Apply(_, param) <- annotations
          } yield constraint(method.tpe, param, tpe)

          PropertyConstraint(memberSymbol,
            propConstraints.reduce(_ && _)
          )
      }) (_ && _)
    }

    def apply(tree: Tree): BoundedType = {
      if(expressionForType.isDefinedAt(tree.tpe)) {
        val f = expressionForType(tree.tpe)
        val exp = f.convertExpression(expression(tree, tree.tpe))
        if(tree.symbol != null && tree.symbol != NoSymbol) {
          BoundedType(Equal(exp) && BoundsFactory.fromSymbolChain(symbolChainFromTree(tree)), f)
        } else {
          BoundedType(Equal(exp), f)
        }
      } else if(tree.symbol != null) {
        BoundedType(BoundsFactory.propertyConstraints(symbolChainFromTree(tree)))
      } else {
        BoundedType.noBounds
      }
    }

    def apply(symbolChain: SymbolChain): BoundedType = {
      BoundedType(fromSymbolChain(symbolChain))
    }

    def fromSymbolChain(symbolChain: SymbolType): Constraint = {
      val list =
        annotatedConstraints(symbolChain.head, symbolChain.head.typeSignature)
      val constraints = (NoConstraints.asInstanceOf[Constraint] /: list) (_&&_)

      ensureLowerAndUpperBounds(constraints, symbolChain.head.typeSignature)
    }

    def fromMethod(
      symbolChain: SymbolChain,
      args: Map[RealSymbolType, BoundedType]): BoundedType = {
        val methodSymbol: MethodSymbol = symbolChain.head.asMethod
        if(expressionForType.isDefinedAt(methodSymbol.returnType)) {
          val annotationConstraints: List[ExpressionConstraint] =
            annotatedConstraints(methodSymbol, methodSymbol.typeSignature)

          val backingFieldConstraint: Constraint =
            if(isStable(methodSymbol))
              Equal(expressionForType(methodSymbol.returnType).
                fromSymbol(symbolChain))
            else
              NoConstraints

          val constraint: List[Constraint] =
            if (methodSymbol.paramss.isEmpty || methodSymbol.paramss.head.isEmpty) {
              annotationConstraints
            } else {
              val argSymbols = methodSymbol.paramss.head
              for {
                ec <- annotationConstraints
                parameter <- ec.expression.extractSymbols.filter(s => argSymbols.contains(s.head))
                paramBounds <- args.get(parameter.head).toList
                paramConstraint <- paramBounds.constraint
              } yield ec.substitute(parameter, paramConstraint).getOrElse(ec)
            }

          BoundedType(
            ensureLowerAndUpperBounds(
              (backingFieldConstraint :: constraint).reduceOption(_&&_).getOrElse(NoConstraints),
              methodSymbol.returnType
          ))

        } else {
          BoundedType(BoundsFactory.propertyConstraints(symbolChain))
        }
      }
  }

  def createConstraintFromSymbol(symbol: SymbolType) = BoundsFactory.fromSymbolChain(symbol)

  abstract class TypeExtractor(expectedType: Type) {
    private val Symbol = expectedType.typeSymbol
    def unapply(tpe: TypeType): Boolean = tpe match {
      case ConstantType(c) if c.tpe <:< expectedType => true
      case TypeRef(_, Symbol, Nil) => true
      case NullaryMethodType(t) if t <:< expectedType => true
      case s@SingleType(_, _) if s <:< expectedType.asInstanceOf[Type] => true
      case _ => false
    }
  }
  object IntTypeExtractor extends TypeExtractor(IntType)
  object LongTypeExtractor extends TypeExtractor(LongType)
  object DoubleTypeExtractor extends TypeExtractor(DoubleType)

  def expressionForType: PartialFunction[TypeType, ExpressionFactory[_]] = {
    case IntTypeExtractor() =>
      new ExpressionFactory[Int](IntType)
    case LongTypeExtractor() =>
      new ExpressionFactory[Long](LongType)
    case DoubleTypeExtractor() =>
      new ExpressionFactory[Double](DoubleType)

    case MethodType(params, IntType) =>
      new MethodExpressionFactory[Int](IntType, params)
    case MethodType(params, DoubleType) =>
      new MethodExpressionFactory[Double](DoubleType, params)
  }

  def symbolChainFromTree(tree: Tree): SymbolChain = {
    def symbolList(tree: Tree): List[RealSymbolType] = tree match {
      case Select(t, n) =>
        findFacadeForSymbol(tree.symbol) :: symbolList(t)
      case _ => List(tree.symbol)
    }
    SymbolChain(symbolList(tree))
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
      case Select(_this,_) =>
        BoundsFactory(tree)
      case Block(body, res) =>
        val newContext = traverseChildren(body)
        val bounds = checkBounds(newContext)(res)
        val blockConstraint = Context.getConstraint(bounds.constraint, res.tpe, newContext)
        BoundedType(blockConstraint)
      case _ => 
        traverseChildren(tree.children)
        BoundedType.noBounds
    }
  }

  def updateContext(context: Context, tree: Tree, constraint: Constraint): Context = tree match {
    case Assign(_, _) => context.removeSymbolConstraints(symbolChainFromTree(tree))
    case _ if constraint != NoConstraints =>
      context && new Context(Map(symbolChainFromTree(tree) -> constraint))
    case _ => context      
  }
}
