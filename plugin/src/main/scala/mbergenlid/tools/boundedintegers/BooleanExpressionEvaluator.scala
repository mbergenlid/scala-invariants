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

  type Factory = (Expression => ExpressionConstraint)
  type Operator = (ExpressionConstraint, ExpressionConstraint) =>
    Option[Expression => ExpressionConstraint]

  private val opToConstraints = Map[Name, (Factory, Factory)](
    n("$less") -> (LessThan, GreaterThan),
    n("$greater") -> (GreaterThan, LessThan),
    n("$less$eq") -> (LessThanOrEqual, GreaterThanOrEqual),
    n("$greater$eq") -> (GreaterThanOrEqual, LessThanOrEqual),
    n("$eq$eq") -> (Equal, Equal)
  )

  type EXP = ExpressionConstraint
  private val operators = Map[Name, (Operator, Operator)](
    n("$less") -> ((_:EXP) <| (_: EXP), (_:EXP) >| (_:EXP)),
    n("$greater") -> ((_:EXP) >| (_:EXP), (_:EXP) <| (_:EXP)),
    n("$less$eq") -> ((_:EXP) <=| (_:EXP), (_:EXP) >=| (_:EXP)),
    n("$greater$eq") -> ((_:EXP) >=| (_:EXP), (_:EXP) <=| (_:EXP)),
    n("$eq$eq") -> ((_:EXP) ==| (_:EXP), (_:EXP) ==| (_:EXP))
  )

  def apply(obj: Context, method: Name, arg: Tree)(implicit c: Context) = method match {
    case a if a == stringToTermName("$amp$amp") => obj && evaluate(arg)(obj && c)
    case a if a == stringToTermName("$bar$bar") => obj || evaluate(arg)
    case _ => obj
  }

  private def apply(lhs: Tree, method: Name, rhs: Tree)(implicit c: Context): Context = {
    val lhsBounds = checkBounds(c)(lhs)
    val rhsBounds = checkBounds(c)(rhs)

    def const(lhs: Constraint, rhs: Constraint, op: Operator) = for {
      ec1 <- lhs.toList
      ec2 <- rhs.toList
      if ec1.expression != ec2.expression
      f <- op.apply(ec1, ec2).toList
      c <-  createConstraints(ec1.expression, ec2.expression, f)
    } yield { c }

    val (rhsOperator, lhsOperator) = operators(method)
    val constraints = const(lhsBounds.constraint, rhsBounds.constraint, rhsOperator) ++
      const(rhsBounds.constraint, lhsBounds.constraint, lhsOperator)

    new Context((Map.empty[SymbolType, Constraint] /: constraints) {(m, t) =>
      m + (t._1 -> (m.getOrElse(t._1, NoConstraints) && t._2))
    })
  }

  private def createConstraints(
    exp1: Expression,
    exp2: Expression,
    factory: Factory): Map[SymbolType, Constraint] = {
      (for {
        symbol <- exp1.extractSymbols
      } yield { symbol -> factory(exp2 + exp1.extractSymbol(symbol)) }).toMap
  }
}
