package mbergenlid.tools.boundedintegers.validators

import mbergenlid.tools.boundedintegers._
import mbergenlid.tools.boundedintegers.facades.TypeFacades

trait MethodApplication extends AbstractBoundsValidator {
  self: MyUniverse with TypeFacades with TypeContext with Expressions with TypeConstraintValidator =>
  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) = 
    validate(context).applyOrElse(tree, super.checkBounds(context))

  private def validate(implicit context: Context): Validator = {
    case Apply(s@Select(_this, method), args) if s.symbol.isMethod =>
      val parameterMap: Map[RealSymbolType, BoundedType] = (for {
        (argSymbol, paramValue) <- extractMethodParams(s, args)
      } yield {
        (argSymbol, argSymbol.withThisSymbol(_this.symbol).tryAssign(paramValue))
      }).toMap
      val _thisBounds: BoundedType = checkBounds(context)(_this)
      BoundsFactory.fromMethod(symbolChainFromTree(s), _thisBounds,
        parameterMap + (MethodExpressionFactory.ThisSymbol -> _thisBounds))

    case t@Apply(method, args) if method.symbol.isMethod =>
      val parameterMap: Map[RealSymbolType, BoundedType] = (for {
        (argSymbol, paramValue) <- extractMethodParams(method, args)
      } yield {
        (argSymbol, argSymbol.tryAssign(paramValue))
      }).toMap

      BoundsFactory.fromMethod(symbolChainFromTree(t), BoundedType.noBounds, parameterMap)
  }

  protected[boundedintegers] 
  def extractMethodParams(methodApplication: Tree, args: List[Tree]): List[(RealSymbolType, Tree)] = {
    val symbol = findFacadeForSymbol(methodApplication.symbol).asMethod
    val res = symbol.paramss.headOption match {
      case Some(list) => list.zip(args)
      case None => Nil
    }
    res
  }
}
