package mbergenlid.tools.boundedintegers.validators

import scala.reflect.api.Universe
import mbergenlid.tools.boundedintegers._

trait Assignment extends AbstractBoundsValidator {
  self: MyUniverse with TypeConstraintValidator =>
  import global._
  import BoundedInteger._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context) _)

  private def validate(implicit context: Context): Validator = {
    case v @ ValDef(_, name, tpe, expr) if(expr != EmptyTree) =>
      v.symbol tryAssign expr
  }
}
