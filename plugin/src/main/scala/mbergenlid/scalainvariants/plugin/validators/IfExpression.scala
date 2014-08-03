package mbergenlid.scalainvariants.plugin.validators


import mbergenlid.scalainvariants.plugin._

trait IfExpression extends BooleanExpressionEvaluator {
  self: MyUniverse =>
  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context))

  private def validate(implicit context: Context): Validator = {
    case If(cond, _then, _else) =>
      val newContext = evaluate(cond)
      val newConstraint =
        checkBounds(context && newContext.evaluate())(Block(Nil, _then)).constraint ||
        checkBounds(context && (!newContext).evaluate())(Block(Nil, _else)).constraint

      BoundedType(newConstraint)
  }
}
