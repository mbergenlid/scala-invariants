package mbergenlid.scalainvariants.api.constraints

import mbergenlid.scalainvariants.api.ApiUniverse

import scala.reflect.api.Symbols

trait ExpressionConstraints {
  self: ApiUniverse =>

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


  case class GreaterThan(expression: Expression) extends ExpressionConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 <= expression
      case GreaterThanOrEqual(v2) => v2.decrement <= expression
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 <= expression
      case LessThanOrEqual(v2) => v2 <= expression
      case Equal(v2) => v2 <= expression
      case _ => false
    }

    override def unary_! = LessThanOrEqual(expression)
    override def prettyPrint(variable: String = "_") =
      s"$variable > $expression"

    override def upperBound = NoConstraints
    override def lowerBound = this

    def map(f: ExpressionConstraint => Expression) =
      GreaterThan(f(this))

    protected[api]
    override def combine(other: ExpressionConstraint) = other match {
      case GreaterThan(v) => Some(GreaterThan)
      case GreaterThanOrEqual(v) => Some(GreaterThan)
      case Equal(v) => Some(GreaterThan)
      case _ => None
    }

    override protected[api]
    def combineNegative(other: ExpressionConstraint) = other match {
      case LessThan(_) => Some(GreaterThan)
      case LessThanOrEqual(_) => Some(GreaterThan)
      case Equal(_) => Some(GreaterThan)
      case _ => None
    }

    override def <|(other: ExpressionConstraint) =
      LessThan(expression) combine other

    override def <=|(other: ExpressionConstraint) = this <| other

    override def ==|(other: ExpressionConstraint) = this <| other
  }

  case class GreaterThanOrEqual(expression: Expression) extends ExpressionConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 < expression
      case GreaterThanOrEqual(v2) => v2 <= expression
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 <= expression
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

  case class LessThanOrEqual(expression: Expression) extends ExpressionConstraint {
    override def definitelySubsetOf(that: Constraint) = that match {
      case LessThan(v2) => v2 > expression
      case LessThanOrEqual(v2) => v2 >= expression
      case _ => super.definitelySubsetOf(that)
    }

    override def definitelyNotSubsetOf(that: Constraint) = that match {
      case GreaterThan(v2) => v2 >= expression
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

   case class PropertyConstraint(
      symbol: SymbolType,
      constraint: Constraint
    ) extends SimpleConstraint {

    override def definitelySubsetOf(that: Constraint) = that match {
      case PropertyConstraint(otherSymbol, otherConstraint) =>
        otherSymbol == symbol && constraint.definitelySubsetOf(otherConstraint)
      case NoConstraints => true
      case _ => false
    }

    def tryAnd(that: SimpleConstraint) = that match {
      case PropertyConstraint(otherSymbol, otherConstraint) if otherSymbol == symbol =>
        Some(copy(constraint = constraint && otherConstraint))
      case _ => None
    }


    def ||(other: Constraint) = other match {
      case PropertyConstraint(sym, c) if sym == symbol =>
        PropertyConstraint(symbol, constraint || c)
      case s: SimpleConstraint => Or(List(And(this), And(s)))
      case a: And => Or(List(And(this), a))
      case Or(ands) => Or(And(this) :: ands)
    }

    def &&(other: Constraint) = other match {
      case PropertyConstraint(sym, c) if sym == symbol =>
        PropertyConstraint(symbol, constraint && c)
      case s: SimpleConstraint => And(List(this, s))
      case And(cs) => And(this :: cs)
      case _ => other && this
    }

    def flatMap(f: (ExpressionConstraint) => Constraint) = ???

    def map[B](f: (ExpressionConstraint) => B)(implicit bf: ConstraintBuilder[B, ExpressionConstraint]) =
      PropertyConstraint(symbol, constraint.map(f))

    def isSymbolConstraint = ???

    def lowerBound = ???

    def upperBound = ???

    def unary_! = ???
  }
}
