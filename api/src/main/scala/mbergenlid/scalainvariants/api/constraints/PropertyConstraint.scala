package mbergenlid.scalainvariants.api.constraints

import scala.reflect.api.Symbols

case class PropertyConstraint(
  symbol: Symbols#SymbolApi,
  constraint: Constraint
) extends SimpleConstraint {

  override def definitelySubsetOf(that: Constraint) = that match {
    case PropertyConstraint(otherSymbol, otherConstraint) =>
      otherSymbol == symbol && constraint.definitelySubsetOf(otherConstraint)
    case NoConstraints => true
    case _ => false
  }

  def tryAnd(constraint: SimpleConstraint) = ???

  def ||(other: Constraint) = ???

  def &&(other: Constraint) = other match {
    case PropertyConstraint(sym, c) if sym == symbol =>
      (constraint && c).map(ec => PropertyConstraint(symbol, ec))
    case s: SimpleConstraint => And(List(this, s))
    case And(cs) => And(this :: cs)
    case _ => other && this
  }

  def flatMap(f: (ExpressionConstraint) => Constraint) = ???

  def map[B](f: (ExpressionConstraint) => B)(implicit bf: ConstraintBuilder[B]) = ???

  def isSymbolConstraint = ???

  def lowerBound = ???

  def upperBound = ???

  def unary_! = ???
}