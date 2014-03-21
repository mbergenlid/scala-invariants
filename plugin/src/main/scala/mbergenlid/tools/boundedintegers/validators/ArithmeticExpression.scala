package mbergenlid.tools.boundedintegers.validators


import mbergenlid.tools.boundedintegers._


trait ArithmeticExpressionValidator extends AbstractBoundsValidator {
  self: MyUniverse =>
  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context))

  private def n(s: String) = stringToTermName(s)

  private val operators = Map[Name, (Expression[Int], Expression[Int]) => Expression[Int]](
    n("$plus") -> (_+_),
    n("$minus") -> (_-_),
    n("$times") -> (_*_)
  )

  /**
   * x < 4
   * y > 5
   *
   * x + y
   * x + 2
   */
  private def validate(implicit context: Context): Validator = {
    case a @ Apply(Select(op1, method), List(op2)) if operators.contains(method) =>
      val lhs = checkBounds(context)(op1)
      Context.getBoundedInteger(
        BoundedInteger(
          Equal(operators(method).apply(BoundsFactory.expression[Int](op1), BoundsFactory.expression[Int](op2)))
        ),
        context
      )
  }
}
