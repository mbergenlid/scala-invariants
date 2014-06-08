package mbergenlid.tools.boundedintegers.validators


import mbergenlid.tools.boundedintegers._


trait ArithmeticExpression extends AbstractBoundsValidator {
  self: MyUniverse =>
  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context))

  private def n(s: String) = stringToTermName(s)

  private val operators = Map[Name, (ExpressionConstraint, ExpressionConstraint) => SimpleConstraint](
    n("$plus") -> (_+_),
    n("$minus") -> (_-_),
    n("$times") -> (_*_)
  )

  /**
   * x < 4
   * y > 5
   *
   *
   * x + y
   * x + 2
   *
   * Int + Int + Double
   */
  private def validate(implicit context: Context): Validator = {
    case a @ Apply(Select(op1, method), List(op2)) if definedForOperator(method, a.tpe) =>
      val lhs = checkBounds(context)(op1).convertTo(a.tpe)
      val rhs = checkBounds(context)(op2).convertTo(a.tpe)

      val newConstraint = for {
        sc1 <- lhs.constraint
        sc2 <- rhs.constraint
      } yield {
        operators(method).apply(sc1, sc2)
      }
      BoundedType(newConstraint)

  }

  private def definedForOperator(op: Name, tpe: TypeType) =
    operators.contains(op) && expressionForType.lift(tpe).isDefined
}
