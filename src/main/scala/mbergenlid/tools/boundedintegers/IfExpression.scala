package mbergenlid.tools.boundedintegers

import scala.reflect.api.Universe

trait IfExpression extends SubTreeValidator { self: MyUniverse =>
  import global._
  import BoundedInteger._

  def validate = {
      case If(cond, _then, _else) => Nil
  }

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
