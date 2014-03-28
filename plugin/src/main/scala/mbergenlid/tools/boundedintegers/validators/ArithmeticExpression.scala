package mbergenlid.tools.boundedintegers.validators


import mbergenlid.tools.boundedintegers._


trait ArithmeticExpressionValidator extends AbstractBoundsValidator {
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
   * x + y
   * x + 2
   *
   * Int + Int + Double
   */
  private def validate(implicit context: Context): Validator = {
    case a @ Apply(Select(op1, method), List(op2)) if operators.contains(method) =>
      val lhs = checkBounds(context)(op1).convertTo(a.tpe)
      val rhs = checkBounds(context)(op2).convertTo(a.tpe)

//      val newConstraint = for {
//        sc1 <- lhs.constraint
//        sc2 <- rhs.constraint
//        f <- Context.createBoundConstraint(sc1, sc2)
//      } yield {
//         val c = f(operators(method).apply(sc1.v, sc2.v))
//         c
//      }
      val newConstraint = lhs.constraint.newFlatMap {sc1 =>
        rhs.constraint.newFlatMap {sc2 =>
          Context.createBoundConstraint(sc1, sc2).map {f =>
            f(operators(method).apply(sc1.v, sc2.v))
          }
        }
      }

      /**
       * x + 4
       *
       * == method && >= 0 && <= 5
       * == 4
       *
       *        == 4 && == method + 4
       *
       * >= 4
       */
//      println(newConstraint.prettyPrint())
//      println(s"\t${lhs.constraint.prettyPrint()}")
//      println(s"\t${rhs.constraint.prettyPrint()}")

      BoundedInteger(newConstraint, a.tpe)
  }
}
