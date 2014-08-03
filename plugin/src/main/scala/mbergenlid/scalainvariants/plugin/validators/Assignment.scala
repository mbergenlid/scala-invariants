package mbergenlid.scalainvariants.plugin.validators

import mbergenlid.scalainvariants.plugin._

trait Assignment extends AbstractBoundsValidator {
  self: MyUniverse with TypeConstraintValidator =>
  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context))

  private def validate(implicit context: Context): Validator = {
    case v @ ValDef(_, name, tpe, expr)
      if expr != EmptyTree =>
      v tryAssign expr
    case Assign(tree, expr) => tree tryAssign expr
  }
}
