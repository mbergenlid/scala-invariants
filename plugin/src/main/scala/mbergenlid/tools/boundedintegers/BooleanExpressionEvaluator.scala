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
    case Apply(Select(obj, method), List(arg)) if opToConstraints.contains(method) =>
      apply(obj, method, arg)
    case _ =>
      checkBounds(c)(expr); new Context
  }

  private def n(s: String) = stringToTermName(s)

  type Factory = (Expression => Constraint)

  private val opToConstraints = Map[Name, (Factory, Factory)](
    n("$less") -> (LessThan, GreaterThan),
    n("$greater") -> (GreaterThan, LessThan),
    n("$less$eq") -> (LessThanOrEqual, GreaterThanOrEqual),
    n("$greater$eq") -> (GreaterThanOrEqual, LessThanOrEqual),
    n("$eq$eq") -> (Equal, Equal)
  )

  def apply(obj: Context, method: Name, arg: Tree)(implicit c: Context) = method match {
    case a if a == stringToTermName("$amp$amp") => obj && evaluate(arg)(obj && c)
    case a if a == stringToTermName("$bar$bar") => obj || evaluate(arg)
    case _ => obj
  }

  private def apply(lhs: Tree, method: Name, rhs: Tree)(implicit c: Context): Context = {
    val lhsBounds = checkBounds(c)(lhs)
    val rhsBounds = checkBounds(c)(rhs)

    val constraints = for {
      exp1 <- lhsBounds.expression.toList
      exp2 <- rhsBounds.expression.toList
      (f1, f2) <- opToConstraints.get(method).toList
      c <- createConstraints(exp1, exp2, f1) ++ createConstraints(exp2, exp1, f2)
    } yield { c }

    new Context(constraints.toMap)
  }

  private def createConstraints(exp1: Expression, exp2: Expression, factory: Factory) = {
    for {
      symbol <- exp1.extractSymbols
    } yield { symbol -> factory(exp2 + exp1.extractSymbol(symbol)) }
  }
}
