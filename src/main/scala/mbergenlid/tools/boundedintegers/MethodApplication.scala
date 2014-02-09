package mbergenlid.tools.boundedintegers

trait MethodApplication extends AbstractBoundsValidator { self: MyUniverse =>
  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) = 
    validate(context).applyOrElse(tree, super.checkBounds(context) _)

  private def validate(context: Context): PartialFunction[Tree, List[BoundedTypeError]] = {
      case Apply(method, args) if(method.symbol.isMethod) => (for {
        (argSymbol, paramValue) <- extractMethodParams(method, args) 
        annotation <- argSymbol.annotations.find { a =>
          a.tpe =:= typeOf[Bounded] && {
            !(getBoundedIntegerFromContext(paramValue, context) <:< BoundedInteger(a))
          }
        }
      } yield { Error(method.pos, createErrorMessage(argSymbol, annotation, paramValue, context)) })
  }

  private def createErrorMessage(argSymbol: Symbol, bounds: Annotation, appliedParam: Tree, context: Context): String = {
    val paramBounds = getBoundedIntegerFromContext(appliedParam, context) 
    s"""
      |Can not assign $appliedParam to $argSymbol.
      |
      |Unable to prove that:
      |  $appliedParam constrained by (${paramBounds.constraint.prettyPrint(appliedParam.toString)})
      |is a subtype of
      |  ${argSymbol.name} constrained by (${BoundedInteger(bounds).constraint.prettyPrint(argSymbol.name.toString)})
    """.stripMargin
  }

  private def getBoundedIntegerFromContext(tree: Tree, context: Context) = {
    context(tree.symbol) match {
      case Some(x) => x
      case None => BoundedInteger(tree)
    }
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
