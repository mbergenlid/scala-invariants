package mbergenlid.scalainvariants.api

trait BoundedTypes {
  self: ApiUniverse =>

  trait BoundedType {
    def constraint: Constraint
  }
  case class NumericType(constraint: Constraint) extends BoundedType {
    protected def this() = this(NoConstraints)

    override def toString = s"NumericType(${constraint.prettyPrint()})"
    override def equals(other: Any) =
      other.isInstanceOf[NumericType] &&
        other.asInstanceOf[NumericType].constraint == this.constraint
  }

  object BoundedType {
    import Constraint._
    def apply(constraint: Constraint, expressionFactory: ExpressionFactory[_]) = {
      NumericType(constraint.map { sc =>
        expressionFactory.convertExpression(sc.expression)
      })
    }

    def apply(constraint: Constraint) = {
      NumericType(constraint)
    }

    def noBounds = NoBounds
  }

  object NoBounds extends BoundedType {
    val constraint = NoConstraints
  }
}
