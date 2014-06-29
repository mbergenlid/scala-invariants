package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe
import mbergenlid.tools.boundedintegers.annotations.{GreaterThanOrEqual => GreaterThanOrEqualAnnotation, LessThan => LessThanAnnotation, LessThanOrEqual => LessThanOrEqualAnnotation, Equal => EqualAnnotation, GreaterThan => GreaterThanAnnotation, Property => PropertyAnnotation, RichNumeric, Bounded}
import mbergenlid.tools.boundedintegers.annotations.RichNumeric.{LongIsRichNumeric, IntIsRichNumeric}
import mbergenlid.tools.boundedintegers.facades.TypeFacades
import scala.reflect.runtime.universe

trait MyUniverse extends Constraints with TypeContext with TypeFacades with Expressions with ExpressionParser {
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
  lazy val GlobalIntType = universe.typeOf[Int]
  lazy val LongType = typeOf[Long]
  lazy val DoubleType = typeOf[Double]
  lazy val IntSymbol = IntType.typeSymbol
  lazy val LongSymbol = LongType.typeSymbol
  lazy val DoubleSymbol = DoubleType.typeSymbol
  lazy val GreaterThanOrEqualType = typeOf[GreaterThanOrEqualAnnotation]
  lazy val EqualType = typeOf[EqualAnnotation]

  abstract class AnnotationExtractor(expectedType: Type, globalType: universe.Type) {
     def unapply(tpe: Type): Boolean = {
       tpe =:= expectedType ||
         tpe.asInstanceOf[universe.Type] <:< globalType
     }
  }
  object GreaterThanOrEqualExtractor extends
    AnnotationExtractor(GreaterThanOrEqualType, universe.typeOf[GreaterThanOrEqualAnnotation])
  object GreaterThanExtractor extends
    AnnotationExtractor(typeOf[GreaterThanAnnotation], universe.typeOf[GreaterThanAnnotation])
  object LessThanOrEqualExtractor extends
    AnnotationExtractor(typeOf[LessThanOrEqualAnnotation], universe.typeOf[LessThanOrEqualAnnotation])
  object LessThanExtractor extends
    AnnotationExtractor(typeOf[LessThanAnnotation], universe.typeOf[LessThanAnnotation])
  object EqualExtractor extends
    AnnotationExtractor(EqualType, universe.typeOf[EqualAnnotation])

  object BoundsFactory {

    private def constraint(bounds: Annotation, symbol: RealSymbolType): ExpressionConstraint =
      constraint(bounds.tpe, bounds.scalaArgs, symbol)

    private def constraint(
      boundType: Type,
      args: List[Tree],
      symbol: RealSymbolType): ExpressionConstraint = boundType match {
        case GreaterThanOrEqualExtractor() =>
          GreaterThanOrEqual(annotationExpression(args.head, symbol))
        case LessThanOrEqualExtractor() =>
          LessThanOrEqual(annotationExpression(args.head, symbol))
        case EqualExtractor() =>
          Equal(annotationExpression(args.head, symbol))
        case LessThanExtractor() =>
          LessThan(annotationExpression(args.head, symbol))
        case GreaterThanExtractor() =>
          GreaterThan(annotationExpression(args.head, symbol))
    }

    private def annotationExpression(tree: Tree, symbol: RealSymbolType): Expression = {
      val Apply(_, List(value)) = tree
      expression(value, symbol)
    }

    private def expression(tree: Tree, symbol: RealSymbolType): Expression = tree match {
      case Literal(Constant(s: String)) =>
        val factory = expressionForType(symbol.typeSignature)
        if(symbol.isMethod)
          factory.fromParameter(s)
        else if(symbol.owner.isMethod)
          factory.withExtraContext(
            symbol.owner.asMethod.paramss.headOption.getOrElse(Nil)).fromParameter(s)
        else
          throw new IllegalArgumentException(s"Symbol $symbol is neither a method nor a method parameter, can not be referred to as a string")
      case _ =>
        expression(tree, symbol.typeSignature)
    }

    def expression(tree: Tree, tpe: TypeType): Expression = tree match {
      case Literal(Constant(x: Int)) =>
        expressionForType(tpe).convertConstant(x)
      case Literal(Constant(x: Long)) =>
        expressionForType(tpe).convertConstant(x)
      case Literal(Constant(x: Double)) =>
        expressionForType(tpe).convertConstant(x)
      case x if x.symbol != NoSymbol =>
        expressionForType(tpe).fromSymbol(symbolChainFromTree(x))
    }

    private def annotatedConstraints(symbol: RealSymbolType, tpe: TypeType): List[ExpressionConstraint] = {
      val annotations = symbol.annotations ++ (
        if(symbol.isMethod && symbol.asMethod.isGetter) symbol.asMethod.accessed.annotations
        else Nil
      )
      val ecs = annotations.collect {
        case a if a.tpe <:< typeOf[Bounded] =>
          BoundsFactory.constraint(a, symbol)
        case a if a.tpe.asInstanceOf[universe.Type] <:< universe.typeOf[Bounded] =>
          BoundsFactory.constraint(a, symbol)
      }
      ecs ++ symbol.allOverriddenSymbols.flatMap(s => annotatedConstraints(s, tpe))
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

    def propertyConstraints(symbolChain: SymbolType): Constraint = {
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


          val propConstraints: List[Constraint] = for {
            method@Apply(_, param) <- annotations
          } yield constraint(method.tpe, param, memberSymbol)

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
          val symbolChain = symbolChainFromTree(tree)
          //Only add -Equal(exp)- if the symbol is stable.
          if(isStable(symbolChain.head))
            BoundedType(Equal(exp) && BoundsFactory.fromSymbolChain(symbolChain), f)
          else
            BoundedType(BoundsFactory.fromSymbolChain(symbolChain))
        } else {
          BoundedType(Equal(exp), f)
        }
      } else if(tree.symbol != null) {
        BoundedType(BoundsFactory.propertyConstraints(symbolChainFromTree(tree)))
      } else {
        BoundedType.noBounds
      }
    }

    def apply(symbolChain: SymbolType): BoundedType = {
      BoundedType(fromSymbolChain(symbolChain))
    }

    def fromSymbolChain(symbolChain: SymbolType): Constraint = {
      val list =
        annotatedConstraints(symbolChain.head, symbolChain.head.typeSignature)
      val constraints = (NoConstraints.asInstanceOf[Constraint] /: list) (_&&_)

      ensureLowerAndUpperBounds(constraints, symbolChain.head.typeSignature)
    }

    def fromMethod(
      symbolChain: SymbolType,
      thisBounds: BoundedType,
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
              val argSymbols = MethodExpressionFactory.ThisSymbol :: methodSymbol.paramss.head

              def substitute(
                symbols: List[(SymbolType, Constraint)],
                ec: ExpressionConstraint): Constraint =
                  symbols match {
                    case (symbol, c) :: rest =>
                      for(sub <- c; result <- ec.substitute(symbol, sub)) yield
                        substitute(rest, result)
                    case Nil =>
                      ec
                  }
              for {
                ec <- annotationConstraints
              } yield {
                val params = ec.expression.extractSymbols.filter(s => argSymbols.contains(s.head)).toList
                val paramConstraints: List[Constraint] =
                  params.map(p => args.get(p.head).map(b => b.constraint).getOrElse(NoConstraints))

                val res = substitute((params zip paramConstraints).toList, ec)
                res
              }
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
      case _ => Symbol.fullName == tpe.typeSymbol.fullName
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

    case MethodType(params, IntTypeExtractor()) =>
      new ExpressionFactory[Int](IntType, params)
    case MethodType(params, DoubleTypeExtractor()) =>
      new ExpressionFactory[Double](DoubleType, params)
  }

  def symbolChainFromTree(tree: Tree): SymbolType = {
    def symbolList(tree: Tree): List[RealSymbolType] = tree match {
      case Select(t, n) =>
        findFacadeForSymbol(tree.symbol) :: symbolList(t)
      case _ => List(tree.symbol)
    }
    SymbolChain[RealSymbolType](symbolList(tree))
  }


  override def parseExpression[T: universe.TypeTag : RichNumeric](
    s: String,
    scope: List[RealSymbolType]): Expression = {

      new ExprParser[T](scope, this).parseExpression(s).get
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
