package mbergenlid.scalainvariants.api

import mbergenlid.scalainvariants.api.Context.ContextTraversable
import mbergenlid.scalainvariants.api.constraints.{PropertyConstraint, NoConstraints, Constraint}
import scala.language.implicitConversions

object Context {

  val Empty = EmptyContext

  implicit def apply(v: (SymbolChain, Constraint)): Context =
    Symbol(v._1, v._2)

  class ContextTraversable(context: Context) extends Traversable[(SymbolChain, Constraint)] {
    override def foreach[U](f: ((SymbolChain, Constraint)) => U): Unit = {
      def foreach(f: ((SymbolChain, Constraint)) => U, c: Context): Unit = c match {
        case lhs && rhs => foreach(f, rhs); foreach(f, lhs)
        case lhs || rhs => foreach(f, rhs); foreach(f, lhs)
        case Symbol(sym, constraint) => f(sym, constraint)
        case EmptyContext =>
      }
      foreach(f, context)
    }
  }
}

trait Context {

  def &&(other: Context): Context = new &&(this, other)

  def ||(other: Context): Context = new ||(this, other)

  def get(symbol: SymbolChain): Constraint

  def -(symbol: SymbolChain): Context

  def symbols: Traversable[(SymbolChain, Constraint)] =
    new ContextTraversable(this)

}

trait ScopedContext extends Context {
  def currentScope: Context
  def pushScope(context: Context): ScopedContext
  def popScope(context: Context): ScopedContext

  override def get(symbol: SymbolChain): Constraint =
    currentScope.get(symbol)

}


case object EmptyContext extends Context {
  override def get(symbol: SymbolChain): Constraint = NoConstraints
  override def -(symbol: SymbolChain): Context = this
}

case class &&(lhs: Context, rhs: Context) extends Context {
  override def get(symbol: SymbolChain): Constraint =
    lhs.get(symbol) && rhs.get(symbol)

  override def -(symbol: SymbolChain): Context =
    new &&(lhs - symbol, rhs - symbol)
}

case class ||(lhs: Context, rhs: Context) extends Context {
  override def get(symbol: SymbolChain): Constraint =
    lhs.get(symbol) || rhs.get(symbol)

  override def -(symbol: SymbolChain): Context =
    new ||(lhs - symbol, rhs - symbol)
}

case class Symbol(symbol: SymbolChain, constraint: Constraint) extends Context {
  override def get(symbol: SymbolChain): Constraint =
    if(this.symbol == symbol) constraint
    else {
      this.symbol.foldSuffixOption(symbol, constraint) { (c, s) =>
        PropertyConstraint(s, c)
      }.getOrElse(NoConstraints)
    }

  override def -(symbol: SymbolChain): Context =
    if(this.symbol.matchesPrefix(symbol)) EmptyContext
    else this
}