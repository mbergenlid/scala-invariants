package mbergenlid.tools.boundedintegers.validators

import mbergenlid.tools.boundedintegers.{TypeConstraintValidator, MyUniverse, AbstractBoundsValidator}

trait MethodDefinition extends AbstractBoundsValidator {
  self: MyUniverse with TypeConstraintValidator =>

  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context))

  def validate(implicit context: Context): Validator = {
    case d @ DefDef(mods, name, _, _, tpt, rhs) =>
      d.symbol tryAssign rhs
  }
}