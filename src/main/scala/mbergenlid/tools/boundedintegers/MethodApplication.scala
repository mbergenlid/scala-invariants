package mbergenlid.tools.boundedintegers

trait MethodApplication extends SubTreeValidator { self: MyUniverse =>
  import global._

  def validate = {
      case Apply(method, args) if(method.symbol.isMethod) => (for {
        (argSymbol, paramValue) <- extractMethodParams(method, args) 
        annotation <- argSymbol.annotations.find { a =>
          a.tpe =:= typeOf[Bounded] &&
            !(BoundedInteger(paramValue) <:< BoundedInteger(a))
        }
      } yield { Error("Failure") })
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
