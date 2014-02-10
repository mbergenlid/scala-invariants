package mbergenlid.tools.boundedintegers

trait MethodApplication extends AbstractBoundsValidator { self: MyUniverse =>
  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) = 
    validate(context).applyOrElse(tree, super.checkBounds(context) _)

  private def validate(implicit context: Context): Validator = {
      case Apply(method, args) if(method.symbol.isMethod) => (for {
        (argSymbol, paramValue) <- extractMethodParams(method, args) 
        errorMessage <- argSymbol.tryAssign(paramValue)
      } { reportError(Error(method.pos, errorMessage)) }); new BoundedInteger
  }

  protected[boundedintegers] 
  def extractMethodParams(methodApplication: Tree, args: List[Tree]): List[(Symbol, Tree)] = {
    val symbol = methodApplication.symbol.asMethod
    symbol.paramss.headOption match {
      case Some(list) => list.zip(args)
      case None => Nil
    }
  }
}
