package mbergenlid.tools.boundedintegers.validators

import mbergenlid.scalainvariants.api.SymbolChain
import mbergenlid.tools.boundedintegers._

trait MethodApplication extends AbstractBoundsValidator with MyUniverse {

  import global._


  abstract override def checkBounds(context: Context)(tree: Tree) = 
    validate(context).applyOrElse(tree, super.checkBounds(context))

  private def validate(implicit context: Context): Validator = {
    case Apply(s@Select(_this, method), args) if s.symbol.isMethod =>
      var updatedContext = context
      val parameterMap: Map[SymbolType, BoundedType] = (for {
        (argSymbol, paramValue) <- extractMethodParams(s, args)
      } yield {
        val paramBounds = argSymbol.withThisSymbol(_this.symbol).tryAssign(paramValue)(updatedContext)
        updatedContext = updatedContext && Context(SymbolChain(List(argSymbol)) -> paramBounds.constraint)
        (argSymbol, paramBounds)
      }).toMap
      val _thisBounds: BoundedType = checkBounds(context)(_this)
      BoundsFactory.fromMethod(symbolChainFromTree(s), _thisBounds,
        parameterMap + (ThisSymbol -> _thisBounds))

    case t@Apply(method, args) if method.symbol.isMethod =>
      var updatedContext = context
      val parameterMap: Map[SymbolType, BoundedType] = (for {
        (argSymbol, paramValue) <- extractMethodParams(method, args)
      } yield {
        val paramBounds = argSymbol.tryAssign(paramValue)(updatedContext)
        updatedContext = updatedContext && (SymbolChain(List(argSymbol)) -> paramBounds.constraint)
        (argSymbol, paramBounds)
      }).toMap

      BoundsFactory.fromMethod(symbolChainFromTree(t), BoundedType.noBounds, parameterMap)
  }

  protected[boundedintegers] 
  def extractMethodParams(
      methodApplication: Tree,
      args: List[Tree]): List[(SymbolType, Tree)] = {

    val symbol = TypeFacade.findFacadeForSymbol(methodApplication.symbol).asMethod
    val res = symbol.paramss.headOption match {
      case Some(list) => list.zip(args)
      case None => Nil
    }
    res
  }
}
