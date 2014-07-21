package mbergenlid.scalainvariants.api.constraints
import mbergenlid.scalainvariants.api.expressions.Expression

case class Equal(expression: Expression) extends ExpressionConstraint {
  override def definitelySubsetOf(that: Constraint) = that match {
    case GreaterThan(v2) => v2 < expression
    case GreaterThanOrEqual(v2) => v2 <= expression
    case LessThan(v2) => v2 > expression
    case LessThanOrEqual(v2) => v2 >= expression
    case Equal(v2) => v2 == expression
    case _ => super.definitelySubsetOf(that)
  }

  override def definitelyNotSubsetOf(that: Constraint) = that match {
    case LessThan(v2) => v2 < expression
    case LessThanOrEqual(v2) => v2 <= expression
    case GreaterThan(v2) => v2 > expression
    case GreaterThanOrEqual(v2) => v2 >= expression
    case Equal(v2) =>
      !(v2.containsSymbols || expression.containsSymbols) &&
        v2 != expression
    case _ => false
  }

  override def unary_! = LessThan(expression) || GreaterThan(expression)
  override def prettyPrint(variable: String = "_") =
    s"$variable == $expression"

  override def upperBound = Equal(expression)
  override def lowerBound = Equal(expression)

  def map(f: ExpressionConstraint => Expression) =
    Equal(f(this))

  protected[api]
  override def combine(other: ExpressionConstraint) = other match {
    case Equal(v) => Some(Equal)
    case _ => other combine this
  }

  override protected[api]
  def combineNegative(other: ExpressionConstraint) = other match {
    case Equal(_) => Some(Equal)
    case LessThan(_) => Some(GreaterThan)
    case LessThanOrEqual(_) => Some(GreaterThanOrEqual)
    case GreaterThan(_) => Some(LessThan)
    case GreaterThanOrEqual(_) => Some(LessThanOrEqual)
    case _ => None
  }

  override def <|(other: ExpressionConstraint) = LessThan(expression).combine(other)
  override def <=|(other: ExpressionConstraint) = LessThanOrEqual(expression).combine(other)
  override def >|(other: ExpressionConstraint) = GreaterThan(expression).combine(other)
  override def >=|(other: ExpressionConstraint) = GreaterThanOrEqual(expression).combine(other)
  override def ==|(other: ExpressionConstraint) = this.combine(other)
}
