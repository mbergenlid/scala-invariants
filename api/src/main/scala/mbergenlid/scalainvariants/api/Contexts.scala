package mbergenlid.scalainvariants.api

import scala.language.implicitConversions

trait Contexts {
  self: ApiUniverse =>

  object Context {

    val Empty = EmptyContext

    implicit def apply(v: (SymbolChain[SymbolType], Constraint)): Context = v._2 match {
      case PropertyConstraint(prop, c) => SymbolContext(prop :: v._1, c)
      case c => SymbolContext(v._1, c)
    }

    class ContextTraversable(context: Context) extends Traversable[(SymbolChain[SymbolType], Constraint)] {
      override def foreach[U](f: ((SymbolChain[SymbolType], Constraint)) => U): Unit = {
        def foreach(f: ((SymbolChain[SymbolType], Constraint)) => U, c: Context): Unit = c match {
          case lhs && rhs => foreach(f, rhs); foreach(f, lhs)
          case lhs || rhs => foreach(f, rhs); foreach(f, lhs)
          case SymbolContext(sym, constraint) => f(sym, constraint)
          case EmptyContext =>
        }
        foreach(f, context)
      }
    }
  }

  trait Context {

    def &&(other: Context): Context = new &&(this, other)

    def ||(other: Context): Context = new ||(this, other)

    def get(symbol: SymbolChain[SymbolType]): Constraint

    def -(symbol: SymbolChain[SymbolType]): Context

    def symbols: Traversable[(SymbolChain[SymbolType], Constraint)] =
      new Context.ContextTraversable(this)

  }

  trait ScopedContext extends Context {
    def currentScope: Context
    def pushScope(context: Context): ScopedContext
    def popScope(context: Context): ScopedContext

    override def get(symbol: SymbolChain[SymbolType]): Constraint =
      currentScope.get(symbol)

  }


  case object EmptyContext extends Context {
    override def get(symbol: SymbolChain[SymbolType]): Constraint = NoConstraints
    override def -(symbol: SymbolChain[SymbolType]): Context = this
  }

  case class &&(lhs: Context, rhs: Context) extends Context {
    override def get(symbol: SymbolChain[SymbolType]): Constraint =
      lhs.get(symbol) && rhs.get(symbol)

    override def -(symbol: SymbolChain[SymbolType]): Context =
      new &&(lhs - symbol, rhs - symbol)
  }

  case class ||(lhs: Context, rhs: Context) extends Context {
    override def get(symbol: SymbolChain[SymbolType]): Constraint =
      lhs.get(symbol) || rhs.get(symbol)

    override def -(symbol: SymbolChain[SymbolType]): Context =
      new ||(lhs - symbol, rhs - symbol)
  }

  case class SymbolContext(symbol: SymbolChain[SymbolType], constraint: Constraint) extends Context {
    override def get(symbol: SymbolChain[SymbolType]): Constraint =
      if(this.symbol == symbol) constraint
      else {
        this.symbol.foldSuffixOption(symbol, constraint) { (c, s) =>
          PropertyConstraint(s, c)
        }.getOrElse(NoConstraints)
      }

    override def -(symbol: SymbolChain[SymbolType]): Context =
      if(this.symbol.matchesPrefix(symbol)) EmptyContext
      else this
  }
}
