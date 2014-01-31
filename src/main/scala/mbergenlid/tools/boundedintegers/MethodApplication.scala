package mbergenlid.tools.boundedintegers

trait MethodApplication extends AbstractBoundsValidator { self: MyUniverse =>
  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) = tree match {
      case Apply(method, args) if(method.symbol.isMethod) => 
        println("METHOD APPLICATION") 
        (for {
        (argSymbol, paramValue) <- extractMethodParams(method, args) 
        annotation <- argSymbol.annotations.find { a =>
          println(a.tpe)
          println(a.tpe =:= typeOf[Bounded])
          a.tpe =:= typeOf[Bounded] &&
            !(BoundedInteger(paramValue) <:< BoundedInteger(a))
        }
      } yield { Error("Failure") })
      case _ => super.checkBounds(context)(tree)
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
