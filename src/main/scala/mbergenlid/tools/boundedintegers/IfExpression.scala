package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe

trait IfExpression extends AbstractBoundsValidator { self: MyUniverse =>
  import global._
  import BoundedInteger._

  abstract override def checkBounds(context: Context)(tree: Tree) =
    validate(context).applyOrElse(tree, super.checkBounds(context) _)

  private def validate(context: Context): Validator = {
    case If(cond, _then, _else) =>
      checkBounds(context ++ createContextFrom(cond))(_then)
  }

  protected[boundedintegers] def
  createContextFrom(cond: Tree): Context = {
    val Apply(Select(a, methodName), List(Literal(Constant(arg: Int)))) = cond

    val bounds = methodName.toString match {
      case "$less" => new BoundedInteger <| arg
      case "$greater" => new BoundedInteger >| arg
      case _ => new BoundedInteger
    }

    new Context(Map(a.symbol -> bounds))
  }
}
