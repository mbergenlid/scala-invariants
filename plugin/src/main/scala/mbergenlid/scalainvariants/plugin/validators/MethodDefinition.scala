package mbergenlid.scalainvariants.plugin.validators

import mbergenlid.scalainvariants.plugin.{TypeConstraintValidator, MyUniverse, AbstractBoundsValidator}

trait MethodDefinition extends AbstractBoundsValidator {
  self: MyUniverse with TypeConstraintValidator =>

  import global._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context))

  private def validate(implicit context: Context): Validator = {
    case d @ DefDef(mods, name, _, _, tpt, rhs) if !mods.hasFlag(Flag.DEFERRED) =>
      d tryAssign rhs
  }
}
