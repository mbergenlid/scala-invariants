package mbergenlid.tools.boundedintegers

trait BooleanExpressionEvaluator { self: MyUniverse =>
  import global._

  import scala.language.implicitConversions
  def n(s: String) = stringToTermName(s)

  val opToConstraints = Map[Name, (Expression => Constraint)](
    n("$less") -> LessThan,
    n("$greater") -> GreaterThan
  )

  val opToBounds = Map[Name, ((BoundedInteger, BoundedInteger) => BoundedInteger)](
    n("$less") -> (_ <| _),
    n("$greater") -> (_ >| _)
  )

  def evaluate(expr: Tree)(implicit c: Context): Context = expr match {
    case Apply(Select(obj, method), List(arg)) if(obj.tpe <:< typeOf[Int] && !obj.symbol.isMethod) => 
      new Context(Map(
        obj.symbol -> apply(BoundedInteger(obj), method, arg).getOrElse(BoundedInteger.noBounds)
      ))
    case Apply(Select(boolExpr, method), List(arg)) if(boolExpr.tpe <:< typeOf[Boolean]) =>
      apply(evaluate(boolExpr), method, evaluate(arg)) 
      
  }

  def apply(obj: Context, method: Name, other: Context) = method match {
    case a if(a == stringToTermName("$amp$amp")) => obj && other
    case a if(a == stringToTermName("$bar$bar")) => obj || other
    case _ => obj
  }

  def apply(obj: BoundedInteger, method: Name, arg: Tree)(implicit c: Context) = {
    val (argExpression, argBounds) = fromTree(arg)(c)
    for {
      constraint <- opToConstraints.get(method)
      op <- opToBounds.get(method)
    } yield { op(obj && BoundedInteger(constraint(argExpression)), argBounds) }
  }

  def fromTree(tree: Tree)(c: Context): (Expression, BoundedInteger) = tree match {
    case Literal(Constant(x: Int)) => (ConstantValue(x), BoundedInteger.noBounds)
    case _ => (SymbolExpression(tree.symbol), c.get(tree.symbol))
  }
}
