package mbergenlid.tools.boundedintegers


trait BooleanExpressionEvaluator extends AbstractBoundsValidator {
  self: MyUniverse =>
  import global._

  def evaluate(expr: Tree)(implicit c: Context): Context = expr match {
    case Apply(Select(obj, method), List(arg)) if obj.tpe <:< typeOf[Int] && !obj.symbol.isMethod =>
      new Context(Map(
        obj.symbol -> apply(BoundsFactory(obj), method, arg).getOrElse(BoundedInteger.noBounds)
      ))
    case Apply(Select(boolExpr, method), List(arg)) if boolExpr.tpe <:< typeOf[Boolean] =>
      apply(evaluate(boolExpr), method, arg) 
    case a @ Apply(method, args) =>
      checkBounds(c)(a); new Context
  }

  private def n(s: String) = stringToTermName(s)

  private val opToConstraints = Map[Name, (Expression[Int] => Constraint)](
    n("$less") -> LessThan,
    n("$greater") -> GreaterThan
  )

  def apply(obj: Context, method: Name, arg: Tree)(implicit c: Context) = method match {
    case a if a == stringToTermName("$amp$amp") => obj && evaluate(arg)(obj && c)
    case a if a == stringToTermName("$bar$bar") => obj || evaluate(arg)
    case _ => obj
  }

  def apply(obj: BoundedInteger, method: Name, arg: Tree)(implicit c: Context) = {
    val argExpression = fromTree(arg)
    for {
      constraint <- opToConstraints.get(method)
    } yield { obj && BoundedInteger(constraint(argExpression)) }
  }

  private def fromTree(tree: Tree): Expression[Int] = tree match {
    case Literal(Constant(x: Int)) => Polynom.fromConstant(x)
    case _ => Polynom.fromSymbol(tree.symbol)
  }
}
