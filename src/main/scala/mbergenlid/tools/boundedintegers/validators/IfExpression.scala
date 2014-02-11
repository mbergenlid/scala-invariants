package mbergenlid.tools.boundedintegers.validators


import mbergenlid.tools.boundedintegers._
import scala.reflect.api.Universe

trait IfExpression extends AbstractBoundsValidator {
  self: MyUniverse with BooleanExpressionEvaluator =>
  import global._
  import BoundedInteger._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context) _)

  private def validate(context: Context): Validator = {
    case If(cond, _then, _else) => {
      val newContext = evaluate(cond)
      checkBounds(context && newContext)(_then) ||
      checkBounds(context && !newContext)(_else)
    }
  }
}
