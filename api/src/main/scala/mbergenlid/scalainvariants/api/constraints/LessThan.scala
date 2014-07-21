package mbergenlid.scalainvariants.api.constraints

import mbergenlid.scalainvariants.api.expressions.Expression


case class LessThan(expression: Expression) extends ExpressionConstraint {
  override def definitelySubsetOf(that: Constraint) = that match {
    case LessThan(v2) => v2 >= expression
    case LessThanOrEqual(v2) =>
      v2.increment >= expression
    case _ => super.definitelySubsetOf(that)
  }

  override def definitelyNotSubsetOf(that: Constraint) = that match {
    case GreaterThan(v2) => v2 >= expression
    case GreaterThanOrEqual(v2) => expression <= v2
    case Equal(v2) => expression <= v2
    case _ => false
  }

  override def unary_! = GreaterThanOrEqual(expression)

  override def prettyPrint(variable: String = "_") =
    s"$variable < ${expression.toString}"

  override def upperBound = this
  override def lowerBound = NoConstraints

  protected[api]
  override def combine(other: ExpressionConstraint) = other match {
    case LessThan(v) => Some(LessThan.apply)
    case LessThanOrEqual(v) => Some(LessThan)
    case Equal(v) => Some(LessThan)
    case _ => None
  }

  def map(f: ExpressionConstraint => Expression) =
    LessThan(f(this))

  override protected[api]
  def combineNegative(other: ExpressionConstraint) = other match {
    case GreaterThan(_) => Some(LessThan)
    case GreaterThanOrEqual(_) => Some(LessThan)
    case Equal(_) => Some(LessThan)
    case _ => None
  }

  override def >|(other: ExpressionConstraint) =
    GreaterThan(expression).combine(other)

  override def >=|(other: ExpressionConstraint) = this >| other

  override def ==|(other: ExpressionConstraint) = this >| other
}