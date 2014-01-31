package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe

trait IfExpression extends AbstractBoundsValidator { self: MyUniverse =>
  import global._
  import BoundedInteger._

  abstract override def checkBounds(context: Context)(tree: Tree) = tree match {
    case If(cond, _then, _else) => super.checkBounds(context)(_then)
      //checkBounds(context)(_then)
    case _ => super.checkBounds(context)(tree)
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
