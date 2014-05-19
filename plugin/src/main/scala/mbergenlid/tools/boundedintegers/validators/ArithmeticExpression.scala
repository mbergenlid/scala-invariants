package mbergenlid.tools.boundedintegers.validators


import mbergenlid.tools.boundedintegers._


trait ArithmeticExpression extends AbstractBoundsValidator {
  self: MyUniverse =>
  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context))

  private def n(s: String) = stringToTermName(s)

  private val operators = Map[Name, (Expression, Expression) => Expression](
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
        f <-  if(method == n("$minus"))
                Context.createNegativeBoundConstraint(sc1, sc2)
              else
                Context.createBoundConstraint(sc1, sc2)
      } yield {
         val c = f(operators(method).apply(sc1.expression, sc2.expression))
         c
      }

      /*
       * x + 4
       *
       * == method && >= 0 && <= 5
       * == 4
       *
       *        == 4 && == method + 4
       *
       * >= 4
       */

      val expression = for {
        exp1 <- lhs.expression
        exp2 <- rhs.expression
      } yield operators(method).apply(exp1, exp2)
      BoundedType(expression, newConstraint)

  }

  private def definedForOperator(op: Name, tpe: TypeType) =
    operators.contains(op) && expressionForType.lift(tpe).isDefined
}
