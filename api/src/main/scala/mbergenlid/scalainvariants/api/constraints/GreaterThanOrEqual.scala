package mbergenlid.scalainvariants.api.constraints

import mbergenlid.scalainvariants.api.expressions.Expression

case class GreaterThanOrEqual(expression: Expression) extends ExpressionConstraint {
  override def definitelySubsetOf(that: Constraint) = that match {
    case GreaterThan(v2) => v2 < expression
    case GreaterThanOrEqual(v2) => v2 <= expression
    case _ => super.definitelySubsetOf(that)
  }

  override def definitelyNotSubsetOf(that: Constraint) = that match {
    case LessThan(v2) => v2 < expression
    case LessThanOrEqual(v2) => v2 < expression
    case Equal(v2) => v2 < expression
    case _ => false
  }

  override def unary_! = LessThan(expression)
  override def prettyPrint(variable: String = "_") =
    s"$variable >= $expression"

  override def upperBound = NoConstraints
  override def lowerBound = GreaterThan(expression)

  def map(f: ExpressionConstraint => Expression) =
    GreaterThanOrEqual(f(this))

  protected[api]
  override def combine(other: ExpressionConstraint) = other match {
    case GreaterThan(v) => Some(GreaterThan)
    case GreaterThanOrEqual(v) => Some(GreaterThanOrEqual)
    case Equal(v) => Some(GreaterThanOrEqual)
    case _ => None
  }

  override protected[api]
  def combineNegative(other: ExpressionConstraint) = other match {
    case LessThan(_) => Some(GreaterThan)
    case LessThanOrEqual(_) => Some(GreaterThanOrEqual)
    case Equal(_) => Some(GreaterThanOrEqual)
    case _ => None
  }

  override def <|(other: ExpressionConstraint) =
    LessThan(expression).combine(other)

  override def <=|(other: ExpressionConstraint) =
    LessThanOrEqual(expression).combine(other)

  override def ==|(other: ExpressionConstraint) = this <=| other
}
