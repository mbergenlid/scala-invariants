package mbergenlid.scalainvariants.api.constraints

import mbergenlid.scalainvariants.api.expressions.Expression

case class LessThanOrEqual(expression: Expression) extends ExpressionConstraint {
  override def definitelySubsetOf(that: Constraint) = that match {
    case LessThan(v2) => v2 > expression
    case LessThanOrEqual(v2) => v2 >= expression
    case _ => super.definitelySubsetOf(that)
  }

  override def definitelyNotSubsetOf(that: Constraint) = that match {
    case GreaterThan(v2) => v2 > expression
    case GreaterThanOrEqual(v2) => v2 > expression
    case Equal(v2) => v2 > expression
    case _ => false
  }

  override def tryAnd(other: SimpleConstraint) = other match {
    case GreaterThanOrEqual(v2) if expression == v2 => Some(Equal(expression))
    case _ => super.tryAnd(other)
  }
  override def unary_! = GreaterThan(expression)

  override def prettyPrint(variable: String = "_") =
    s"$variable <= $expression"

  override def upperBound = LessThan(expression)
  override def lowerBound = NoConstraints

  protected[api]
  override def combine(other: ExpressionConstraint) = other match {
    case LessThan(v) => Some(LessThan)
    case LessThanOrEqual(v) => Some(LessThanOrEqual)
    case Equal(v) => Some(LessThanOrEqual)
    case _ => None
  }

  def map(f: ExpressionConstraint => Expression) =
    LessThanOrEqual(f(this))

  override protected[api]
  def combineNegative(other: ExpressionConstraint) = other match {
    case GreaterThan(_) => Some(LessThan)
    case GreaterThanOrEqual(_) => Some(LessThanOrEqual)
    case Equal(_) => Some(LessThanOrEqual)
    case _ => None
  }

  override def >|(other: ExpressionConstraint) =
    GreaterThan(expression).combine(other)

  override def >=|(other: ExpressionConstraint) =
    GreaterThanOrEqual(expression).combine(other)

  override def ==|(other: ExpressionConstraint) = this >=| other
}
