package mbergenlid.scalainvariants.api

import mbergenlid.scalainvariants.api.constraints.{PropertyConstraint, NoConstraints, Constraint}
import scala.language.implicitConversions

object Context {

  val Empty = EmptyContext

  implicit def apply(v: (SymbolChain, Constraint)): Context =
    Symbol(v._1, v._2)
}

trait Context {

  def &&(other: Context): Context = new &&(this, other)

  def ||(other: Context): Context = new ||(this, other)

  def get(symbol: SymbolChain): Constraint
}

object EmptyContext extends Context {
  override def get(symbol: SymbolChain): Constraint = NoConstraints
}

case class &&(lhs: Context, rhs: Context) extends Context {
  override def get(symbol: SymbolChain): Constraint =
    lhs.get(symbol) && rhs.get(symbol)
}

case class ||(lhs: Context, rhs: Context) extends Context {
  override def get(symbol: SymbolChain): Constraint =
    lhs.get(symbol) || rhs.get(symbol)
}

case class Symbol(symbol: SymbolChain, constraint: Constraint) extends Context {
  override def get(symbol: SymbolChain): Constraint =
    if(this.symbol == symbol) constraint
    else {
      this.symbol.foldSuffixOption(symbol, constraint) { (c, s) =>
        PropertyConstraint(s, c)
      }.getOrElse(NoConstraints)
    }

}