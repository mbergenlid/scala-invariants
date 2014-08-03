package mbergenlid.scalainvariants.plugin

import mbergenlid.scalainvariants.api.SymbolChain

import scala.language.existentials

trait BooleanExpressionEvaluator extends AbstractBoundsValidator {
  self: MyUniverse =>
  import global._

  object BoolOperators {
    trait BoolOperator {
      def evaluate(): Context
      def unary_! :BoolOperator
    }

    trait Operator2 extends BoolOperator {

      def lhs: Constraint
      def rhs: Constraint
      def operator: Operator
      def reverseOperator: Operator

      override def evaluate(): Context = {
        val constraints = const(lhs, rhs, operator) ++
          const(rhs, lhs, reverseOperator)

        constraints.foldLeft[Context](EmptyContext)(_&&_)
      }

      protected[BooleanExpressionEvaluator]
      def const(
        lhs: Constraint,
        rhs: Constraint,
        op: Operator): List[(SymbolChain[SymbolType], Constraint)] = for {
        ec1 <- lhs.toList
        ec2 <- rhs.toList
        f <- op.apply(ec1, ec2).toList
        c <-  createConstraints(ec1.expression, ec2.expression, f)
      } yield { c }
    }

    case object EmptyOperator extends BoolOperator {
      override def evaluate() = EmptyContext
      override def unary_! = this
    }
    case class &&(lhs: BoolOperator, rhs: BoolOperator) extends BoolOperator {
      override def evaluate() = lhs.evaluate() && rhs.evaluate()
      override def unary_! = ||(!lhs, !rhs)
    }
    case class ||(lhs: BoolOperator, rhs: BoolOperator) extends BoolOperator {
      override def evaluate() = lhs.evaluate() || rhs.evaluate()
      override def unary_! = &&(!lhs, !rhs)
    }
    case class <|(lhs: Constraint, rhs: Constraint) extends Operator2 {
      override val operator = (_:EXP)<|(_:EXP)
      override val reverseOperator = (_:EXP)>|(_:EXP)
      override def unary_! = >=|(lhs, rhs)
    }
    case class <=|(lhs: Constraint, rhs: Constraint) extends Operator2 {
      override val operator = (_:EXP)<=|(_:EXP)
      override val reverseOperator = (_:EXP)>|(_:EXP)
      override def unary_! = >|(lhs, rhs)
    }
    case class >|(lhs: Constraint, rhs: Constraint) extends Operator2 {
      override val operator = (_:EXP)>|(_:EXP)
      override val reverseOperator = (_:EXP)<=|(_:EXP)
      override def unary_! = <=|(lhs, rhs)
    }
    case class >=|(lhs: Constraint, rhs: Constraint) extends Operator2 {
      override val operator: Operator = _ >=| _
      override val reverseOperator: Operator = _<=|_
      override def unary_! : BoolOperator = <|(lhs, rhs)
    }
    case class ==|(lhs: Constraint, rhs: Constraint) extends Operator2 {
      override val operator = (_:EXP)==|(_:EXP)
      override def reverseOperator = operator
      override def unary_! = ||(<|(lhs, rhs), >|(lhs, rhs))
    }
  }

  import BoolOperators._

  class DeferredBoolExpression(method: Name, expression: Name => Context) {
    def apply(): Context = expression(method)
    def unary_! :DeferredBoolExpression = method match {
      case m if m == n("$less") => new DeferredBoolExpression(n("$greater$eq"), expression)
    }
  }

  def evaluate(expr: Tree)(implicit c: Context): BoolOperator = expr match {
    case Apply(Select(boolExpr, method), List(arg)) if boolExpr.tpe <:< typeOf[Boolean] =>
      apply(evaluate(boolExpr), method, arg)
    case Apply(Select(obj, method), List(arg)) if operators.contains(method) =>
      apply(obj, method, arg)
    case _ =>
      checkBounds(c)(expr)
      EmptyOperator
  }

  private def n(s: String) = stringToTermName(s)

  type Factory = (Expression => ExpressionConstraint)
  type OpFactory = (Constraint, Constraint) => Operator2
  type Operator = (ExpressionConstraint, ExpressionConstraint) =>
    Option[Expression => ExpressionConstraint]

  type EXP = ExpressionConstraint
  private val operators = Map[Name, OpFactory](
    n("$less") -> {(lhs, rhs) => <|(lhs, rhs)},
    n("$greater") -> {(lhs, rhs) => >|(lhs, rhs)},
    n("$less$eq") -> {(lhs, rhs) => <=|(lhs, rhs)},
    n("$greater$eq") -> {(lhs, rhs) => >=|(lhs, rhs)},
    n("$eq$eq") -> {(lhs, rhs) => ==|(lhs, rhs)}
  )

  def apply(obj: BoolOperator, method: Name, arg: Tree)(implicit c: Context): BoolOperator = method match {
    case a if a == stringToTermName("$amp$amp") => BoolOperators.&&(obj, evaluate(arg)(obj.evaluate() && c))
    case a if a == stringToTermName("$bar$bar") => BoolOperators.||(obj, evaluate(arg))
    case _ => obj
  }

  private def apply(lhs: Tree, method: Name, rhs: Tree)(implicit c: Context): Operator2 = {
    val lhsBounds = checkBounds(c)(lhs)
    val rhsBounds = checkBounds(c)(rhs)

    operators(method)(lhsBounds.constraint, rhsBounds.constraint)
  }

  private def createConstraints(
    exp1: Expression,
    exp2: Expression,
    factory: Factory): Map[SymbolChain[SymbolType], Constraint] = {
      (for {
        symbol <- exp1.extractSymbols
      } yield { symbol -> factory(exp2 + exp1.extractSymbol(symbol)) }).toMap
  }
}
