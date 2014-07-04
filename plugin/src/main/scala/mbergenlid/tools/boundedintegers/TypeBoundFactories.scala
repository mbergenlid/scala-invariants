package mbergenlid.tools.boundedintegers

import mbergenlid.tools.boundedintegers.annotations.{Equal => EqualAnnotation, GreaterThan => GreaterThanAnnotation, GreaterThanOrEqual => GreaterThanOrEqualAnnotation, LessThan => LessThanAnnotation, LessThanOrEqual => LessThanOrEqualAnnotation, Property => PropertyAnnotation, Bounded}
import mbergenlid.tools.boundedintegers.facades.TypeFacades

import scala.reflect.runtime._

trait TypeBoundFactories {
  self: MyUniverse with Constraints with Expressions with TypeFacades with BoundedTypes =>

  import global._

  lazy val GreaterThanOrEqualType = typeOf[GreaterThanOrEqualAnnotation]
  lazy val EqualType = typeOf[EqualAnnotation]

  def createConstraintFromSymbol(symbol: SymbolType) = BoundsFactory.fromSymbolChain(symbol)

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

  trait BoundedTypeFactory {
    def fromSymbolChain(symbol: SymbolType): Constraint
    def fromTree(tree: Tree): BoundedType
    def fromMethodApplication(
      symbolChain: SymbolType,
      thisBounds: BoundedType,
      args: Map[RealSymbolType, BoundedType]): BoundedType
  }

  object BoundsFactory {

    private def getFactory(tpe: Type): BoundedTypeFactory =
      if(expressionForType.isDefinedAt(tpe)) NumericBoundsFactory
      else PropertyBoundsFactory


    def fromSymbolChain(symbol: SymbolType): Constraint =
      getFactory(symbol.head.typeSignature).fromSymbolChain(symbol)

    def fromMethod(
      symbolChain: SymbolType,
      thisBounds: BoundedType,
      args: Map[RealSymbolType, BoundedType]): BoundedType = {

        val methodSymbol: MethodSymbol = symbolChain.head.asMethod
        getFactory(methodSymbol.returnType).fromMethodApplication(symbolChain, thisBounds, args)
    }


    def apply(tree: Tree): BoundedType =
      getFactory(tree.tpe).fromTree(tree)

    //Temporary
    def propertyConstraints(symbolChain: SymbolType): Constraint =
      PropertyBoundsFactory.fromSymbolChain(symbolChain)
  }

  object NumericBoundsFactory extends BoundedTypeFactory {


    private def constraint(bounds: Annotation, symbol: RealSymbolType): ExpressionConstraint =
      constraint(bounds.tpe, bounds.scalaArgs, symbol)

    private[TypeBoundFactories]
    def constraint(
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
        if(isStable(symbolChain.head))
          BoundedType(Equal(exp) && fromSymbolChain(symbolChain), f)
        else
          BoundedType(fromSymbolChain(symbolChain))
      } else {
        BoundedType(Equal(exp), f)
      }
    }

    def fromSymbolChain(symbolChain: SymbolType): Constraint = {
      val list =
        annotatedConstraints(symbolChain.head, symbolChain.head.typeSignature)
      val constraints = (NoConstraints.asInstanceOf[Constraint] /: list) (_&&_)

      ensureLowerAndUpperBounds(constraints, symbolChain.head.typeSignature)
    }

    def fromMethodApplication(
      symbolChain: SymbolType,
      thisBounds: BoundedType,
      args: Map[RealSymbolType, BoundedType]): BoundedType = {
        val methodSymbol: MethodSymbol = symbolChain.head.asMethod
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
    }

  }

  object PropertyBoundsFactory extends BoundedTypeFactory {
    override def fromSymbolChain(symbolChain: SymbolType): Constraint = {
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
          } yield NumericBoundsFactory.constraint(method.tpe, param, memberSymbol)

          PropertyConstraint(memberSymbol,
            propConstraints.reduce(_ && _)
          )
      }) (_ && _)
    }
    override def fromTree(tree: Tree): BoundedType =
      if(tree.symbol != null)
        BoundedType(BoundsFactory.propertyConstraints(symbolChainFromTree(tree)))
      else
        BoundedType.noBounds

    override def fromMethodApplication(
      symbolChain: SymbolType,
      thisBounds: BoundedType,
      args: Map[RealSymbolType, BoundedType]): BoundedType =
        BoundedType(fromSymbolChain(symbolChain))
  }
}
