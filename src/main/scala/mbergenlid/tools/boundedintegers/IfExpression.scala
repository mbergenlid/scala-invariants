package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe

trait IfExpression extends AbstractBoundsValidator {
  self: MyUniverse with BooleanExpressionEvaluator =>
  import global._
  import BoundedInteger._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context) _)

  private def validate(context: Context): Validator = {
    case If(cond, _then, _else) => {
      checkBounds(context ++ evaluate(cond))(_then)
    }
  }
}
