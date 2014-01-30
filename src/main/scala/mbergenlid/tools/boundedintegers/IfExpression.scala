package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe

class IfExpression(implicit val global: Universe) extends MyUniverse {
  import global._
  import BoundedInteger._

  //def apply(tree: Tree)
  protected[boundedintegers] def
  createContextFrom(cond: Tree): Map[Symbol, BoundedInteger] = {
    val Apply(Select(a, methodName), List(Literal(Constant(arg: Int)))) = cond

    val bounds = methodName.toString match {
      case "$less" => new BoundedInteger <| arg
      case "$greater" => new BoundedInteger >| arg
      case _ => new BoundedInteger
    }

    Map(a.symbol -> bounds)
  }
}
