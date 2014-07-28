package mbergenlid.tools.boundedintegers

import mbergenlid.scalainvariants.api.{ApiUniverse, SymbolChain}
import mbergenlid.tools.boundedintegers.annotations.{Equal => EqualAnnotation, GreaterThan => GreaterThanAnnotation, GreaterThanOrEqual => GreaterThanOrEqualAnnotation, LessThan => LessThanAnnotation, LessThanOrEqual => LessThanOrEqualAnnotation, Property => PropertyAnnotation, Bounded}

import scala.reflect.runtime._

trait TypeBoundFactories extends ApiUniverse {
  self: MyUniverse =>

  import global._

  lazy val GreaterThanOrEqualType = universe.typeOf[GreaterThanOrEqualAnnotation].asInstanceOf[Type]
  lazy val GreaterThanType = universe.typeOf[GreaterThanAnnotation].asInstanceOf[Type]
  lazy val EqualType = universe.typeOf[EqualAnnotation].asInstanceOf[Type]
  lazy val LessThanOrEqualType = universe.typeOf[LessThanOrEqualAnnotation].asInstanceOf[Type]
  lazy val LessThanType = universe.typeOf[LessThanAnnotation].asInstanceOf[Type]
  lazy val ThisSymbol =
    typeOf[String].termSymbol.newTermSymbol(newTermName("this")).asInstanceOf[SymbolType]

  override def createConstraintFromSymbol(symbol: SymbolType) = BoundsFactory.fromSymbol(symbol)

  abstract class AnnotationExtractor(expectedType: Type, globalType: universe.Type) {
    def unapply(tpe: Type): Boolean = {
      tpe =:= expectedType ||
        tpe.asInstanceOf[universe.Type] <:< globalType
    }
  }

  object GreaterThanOrEqualExtractor extends
    AnnotationExtractor(GreaterThanOrEqualType, universe.typeOf[GreaterThanOrEqualAnnotation])
  object GreaterThanExtractor extends
    AnnotationExtractor(GreaterThanType, universe.typeOf[GreaterThanAnnotation])
  object LessThanOrEqualExtractor extends
    AnnotationExtractor(LessThanOrEqualType, universe.typeOf[LessThanOrEqualAnnotation])
  object LessThanExtractor extends
    AnnotationExtractor(LessThanType, universe.typeOf[LessThanAnnotation])
  object EqualExtractor extends
    AnnotationExtractor(EqualType, universe.typeOf[EqualAnnotation])

  trait BoundedTypeFactory {
    def fromSymbol(symbol: SymbolType): Constraint
    def fromTree(tree: Tree): BoundedType
    def fromMethodApplication(
      symbolChain: SymbolChain[SymbolType],
      thisBounds: BoundedType,
      args: Context): BoundedType
  }

  object BoundsFactory {

    private def getFactory(tpe: Type): BoundedTypeFactory =
      if(expressionForType.isDefinedAt(tpe)) NumericBoundsFactory
      else PropertyBoundsFactory


    def fromSymbol(symbol: SymbolType): Constraint =
      getFactory(symbol.typeSignature).fromSymbol(symbol)

    def fromSymbolChain(symbol: SymbolChain[SymbolType]): Constraint =
      fromSymbol(symbol.head)

    def fromMethod(
      symbolChain: SymbolChain[SymbolType],
      thisBounds: BoundedType,
      args: Context): BoundedType = {

        val methodSymbol: MethodSymbol = symbolChain.head.asMethod
        getFactory(methodSymbol.returnType).fromMethodApplication(symbolChain, thisBounds, args)
    }


    def apply(tree: Tree): BoundedType =
      getFactory(tree.tpe).fromTree(tree)
  }

  object NumericBoundsFactory extends BoundedTypeFactory {


    private def constraint(bounds: Annotation, symbol: SymbolType): ExpressionConstraint =
      constraint(bounds.tpe, bounds.scalaArgs, symbol)

    private[TypeBoundFactories]
    def constraint(
      boundType: Type,
      args: List[Tree],
      symbol: SymbolType): ExpressionConstraint = boundType match {
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

    private def annotationExpression(tree: Tree, symbol: SymbolType): Expression = {
      val Apply(_, List(value)) = tree
      expression(value, symbol)
    }

    private def expression(tree: Tree, symbol: SymbolType): Expression = tree match {
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

    private def annotatedConstraints(symbol: SymbolType, tpe: TypeType): List[ExpressionConstraint] = {
      val annotations = symbol.annotations ++ (
        if(symbol.isMethod && symbol.asMethod.isGetter) symbol.asMethod.accessed.annotations
        else Nil
        )
      val ecs = annotations.collect {
        case a if a.tpe <:< universe.typeOf[Bounded].asInstanceOf[Type] =>
          constraint(a, symbol)
        case a if a.tpe.asInstanceOf[universe.Type] <:< universe.typeOf[Bounded] =>
          constraint(a, symbol)
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

    def fromTree(tree: Tree): BoundedType = {
      val f = expressionForType(tree.tpe)
      val exp = expression(tree, tree.tpe)
      if(tree.symbol != null && tree.symbol != NoSymbol) {
        val symbolChain = symbolChainFromTree(tree)
        //Only add -Equal(exp)- if the symbol is stable.
        if(symbolChain.isStable)
          BoundedType(Equal(exp) && fromSymbol(symbolChain.head), f)
        else
          BoundedType(fromSymbol(symbolChain.head))
      } else {
        BoundedType(Equal(exp), f)
      }
    }

    def fromSymbol(symbolChain: SymbolType): Constraint = {
      val list =
        annotatedConstraints(symbolChain, symbolChain.typeSignature)
      val constraints = (NoConstraints.asInstanceOf[Constraint] /: list) (_&&_)

      ensureLowerAndUpperBounds(constraints, symbolChain.typeSignature)
    }

    def fromMethodApplication(
      symbolChain: SymbolChain[SymbolType],
      thisBounds: BoundedType,
      args: Context): BoundedType = {
        val methodSymbol: MethodSymbol = symbolChain.head.asMethod
        val annotationConstraints: List[ExpressionConstraint] =
          annotatedConstraints(methodSymbol, methodSymbol.typeSignature)

        val backingFieldConstraint: Constraint =
          if(SymbolChain(List(methodSymbol)).isStable)
            Equal(expressionForType(methodSymbol.returnType).
              fromSymbol(symbolChain))
          else
            NoConstraints

        val constraint: List[Constraint] =
          if (methodSymbol.paramss.isEmpty || methodSymbol.paramss.head.isEmpty) {
            annotationConstraints
          } else {

            def substitute(
              symbols: List[(SymbolChain[SymbolType], Constraint)],
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
              val params = ec.expression.extractSymbols.map { s =>
                (s, args.get(s))
              }

              val res = substitute(params.toList, ec)
              res
            }
          }

        BoundedType(
          ensureLowerAndUpperBounds(
            (backingFieldConstraint :: constraint).reduceOption(_&&_).getOrElse(NoConstraints),
            methodSymbol.returnType
          ))
    }

  }

  object PropertyBoundsFactory extends BoundedTypeFactory {
    override def fromSymbol(symbol: SymbolType): Constraint = {
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

          if(!SymbolChain(List(memberSymbol)).isStable)
            throw new CompilationError(
              Error(t.pos, s"Can not be bound to property $prop in type ${symbol.typeSignature} as " +
                s"it is not stable."))

          if(annotations.isEmpty)
            throw new CompilationError(
              Error(t.pos, "@Property requires at least one constraint"))


          val propConstraints: List[Constraint] = for {
            method@Apply(_, param) <- annotations
          } yield NumericBoundsFactory.constraint(method.tpe, param, memberSymbol)

          PropertyConstraint(memberSymbol,
            propConstraints.reduce(_ && _)
          )
      }) (_ && _)
    }
    override def fromTree(tree: Tree): BoundedType =
      if(tree.symbol != null) {
        val chain = symbolChainFromTree(tree)
        BoundedType(
          propertyConstraints(chain) &&
          fromSymbol(chain.head)
        )
      } else
        BoundedType.noBounds

    private def propertyConstraints(symbolChain: SymbolChain[SymbolType]): Constraint = {
      def isPublicStable(s: Symbol) = s.isPublic && s.isTerm && s.asTerm.isStable

      val symbol = symbolChain.head
      val stableMembers = TypeFacade.findFacadeForType(symbol.typeSignature).members.filter(isPublicStable)
      val constraints: Traversable[Constraint] = stableMembers.map {s: Symbol =>
        if(expressionForType.isDefinedAt(s.typeSignature))
          PropertyConstraint(s,
            Equal(expressionForType(s.typeSignature).fromSymbol(SymbolChain(s :: symbolChain.symbols))))
        else
          NoConstraints
      }
      constraints.reduceOption(_&&_).getOrElse(NoConstraints)
    }


    override def fromMethodApplication(
      symbolChain: SymbolChain[SymbolType],
      thisBounds: BoundedType,
      args: Context): BoundedType =
        BoundedType(fromSymbol(symbolChain.head))
  }
}
