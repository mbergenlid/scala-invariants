package mbergenlid.tools.boundedintegers
import scala.language.existentials

trait BooleanExpressionEvaluator extends AbstractBoundsValidator {
  self: MyUniverse =>
  import global._

  /**
   *  x < 10
   *  x + 1 < 10
   *
   */
  def evaluate(expr: Tree)(implicit c: Context): Context = expr match {
    case Apply(Select(boolExpr, method), List(arg)) if boolExpr.tpe <:< typeOf[Boolean] =>
      apply(evaluate(boolExpr), method, arg)
    case Apply(Select(obj, method), List(arg)) if opToConstraints.contains(method) && !obj.symbol.isMethod =>
      new Context(Map(
        obj.symbol -> apply(BoundsFactory(obj), method, arg).getOrElse(NoConstraints)
      ))
    case _ =>
      checkBounds(c)(expr); new Context
  }

  private def n(s: String) = stringToTermName(s)

  type Factory = (Expression => Constraint)

  private val opToConstraints = Map[Name, Factory](
    n("$less") -> LessThan,
    n("$greater") -> GreaterThan
  )

  def apply(obj: Context, method: Name, arg: Tree)(implicit c: Context) = method match {
    case a if a == stringToTermName("$amp$amp") => obj && evaluate(arg)(obj && c)
    case a if a == stringToTermName("$bar$bar") => obj || evaluate(arg)
    case _ => obj
  }

  def apply(obj: BoundedType, method: Name, arg: Tree)(implicit c: Context): Option[Constraint] = {
    val argExpression = BoundsFactory.expression(arg, arg.tpe)
    for {
      constraint <- opToConstraints.get(method)
    } yield { obj.constraint && constraint(argExpression) }
  }

}
