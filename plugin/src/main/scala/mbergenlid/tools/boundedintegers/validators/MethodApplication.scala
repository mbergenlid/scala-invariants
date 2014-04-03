package mbergenlid.tools.boundedintegers.validators

import mbergenlid.tools.boundedintegers._

trait MethodApplication extends AbstractBoundsValidator {
  self: MyUniverse with TypeConstraintValidator =>
  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) = 
    validate(context).applyOrElse(tree, super.checkBounds(context) _)

  private def validate(implicit context: Context): Validator = {
      case Apply(method, args) if method.symbol.isMethod =>
        for {
          (argSymbol, paramValue) <- extractMethodParams(method, args)
        } {
          argSymbol.tryAssign(paramValue)
        }; BoundsFactory(method)
  }

  protected[boundedintegers] 
  def extractMethodParams(methodApplication: Tree, args: List[Tree]): List[(Symbol, Tree)] = {
    val symbol = methodApplication.symbol.asMethod
    val res = symbol.paramss.headOption match {
      case Some(list) => list.zip(args)
      case None => Nil
    }
    res
  }
}
