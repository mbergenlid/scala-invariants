package mbergenlid.tools.boundedintegers

trait BooleanExpressionEvaluator extends AbstractBoundsValidator {
  self: MyUniverse =>
  import global._

  def evaluate(expr: Tree)(implicit c: Context): Context = expr match {
    case Apply(Select(obj, method), List(arg)) if(obj.tpe <:< typeOf[Int] && !obj.symbol.isMethod) => 
      new Context(Map(
        obj.symbol -> apply(BoundedInteger(obj), method, arg).getOrElse(BoundedInteger.noBounds)
      ))
    case Apply(Select(boolExpr, method), List(arg)) if(boolExpr.tpe <:< typeOf[Boolean]) =>
      apply(evaluate(boolExpr), method, arg) 
    case a @ Apply(method, args) =>
      checkBounds(c)(a); new Context
  }

  def n(s: String) = stringToTermName(s)

  val opToConstraints = Map[Name, (Expression => Constraint)](
    n("$less") -> LessThan,
    n("$greater") -> GreaterThan
  )

  val opToBounds = Map[Name, ((BoundedInteger, BoundedInteger) => BoundedInteger)](
    n("$less") -> (_ <| _),
    n("$greater") -> (_ >| _)
  )

  def apply(obj: Context, method: Name, arg: Tree)(implicit c: Context) = method match {
    case a if(a == stringToTermName("$amp$amp")) => obj && (evaluate(arg)(obj && c))
    case a if(a == stringToTermName("$bar$bar")) => obj || (evaluate(arg))
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
    case _ => (SymbolExpression(tree.symbol), c(tree.symbol).getOrElse(BoundedInteger(tree)))
  }
}
