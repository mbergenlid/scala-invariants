package mbergenlid.tools.boundedintegers

trait BooleanExpressionEvaluator { self: MyUniverse =>
  import global._

  def evaluate(expr: Tree): Context = expr match {
    case Apply(Select(obj, method), List(arg)) if(obj.tpe <:< typeOf[Int] && !obj.symbol.isMethod) => 
      new Context(Map(
        obj.symbol -> apply(BoundedInteger(obj), method, arg)
      ))
    case Apply(Select(boolExpr, method), List(arg)) if(boolExpr.tpe <:< typeOf[Boolean]) =>
      apply(evaluate(boolExpr), method, evaluate(arg)) 
      
  }

  def apply(obj: Context, method: Name, other: Context) = method match {
    case a if(a == stringToTermName("$amp$amp")) => obj && other
    case a if(a == stringToTermName("$bar$bar")) => obj || other
    case _ => obj
  }

  def apply(obj: BoundedInteger, method: Name, arg: Tree) = {
    method match {
      case a if(a == stringToTermName("$less")) => obj && BoundedInteger(LessThan(fromTree(arg)))
      case a if(a == stringToTermName("$greater")) => obj && BoundedInteger(GreaterThan(fromTree(arg)))
      case _ => obj
    }
  }

  def fromTree(tree: Tree) = tree match {
    case Literal(Constant(x: Int)) => ConstantValue(x)
    case _ => SymbolExpression(tree.symbol)
  }
}
