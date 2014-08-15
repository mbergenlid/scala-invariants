package mbergenlid.scalainvariants.api

import scala.language.implicitConversions

trait Contexts {
  self: ApiUniverse =>

  import Constraint._
  object Context {

    val EmptyContext = new Context(NoConstraints)

    implicit def apply(v: (SymbolChain[SymbolType], Constraint)): Context = v._2 match {
      case PropertyConstraint(prop, c) => Context(SymbolContext(prop :: v._1, c))
      case And(cs) => cs.foldLeft[Context](EmptyContext)((c1, c2) => c1 && apply(v._1, c2))
      case c => Context(SymbolContext(v._1, c))
    }
//
//    class ContextTraversable(context: Context) extends Traversable[(SymbolChain[SymbolType], Constraint)] {
//      override def foreach[U](f: ((SymbolChain[SymbolType], Constraint)) => U): Unit = {
//        def foreach(f: ((SymbolChain[SymbolType], Constraint)) => U, c: Context): Unit = c match {
//          case lhs && rhs => foreach(f, rhs); foreach(f, lhs)
//          case lhs || rhs => foreach(f, rhs); foreach(f, lhs)
//          case SymbolContext(sym, constraint) => f(sym, constraint)
//          case EmptyContext =>
//        }
//        foreach(f, context)
//      }
//    }
  }

  case class Context(constraint: Constraint) {

    def &&(other: Context): Context =
      Context(this.constraint && other.constraint)

    def ||(other: Context): Context =
      Context(this.constraint || other.constraint)

    def get(symbol: SymbolChain[SymbolType]): Constraint =
      constraint.mapSimpleConstraints {
        case SymbolContext(sym, c) if sym == symbol => c
        case SymbolContext(sym, symbolConstraint) => sym.foldSuffixOption(symbol, symbolConstraint) { (c, s) =>
          PropertyConstraint(s, c)
        }.getOrElse(NoConstraints)
        case c => c
      }

    def -(symbol: SymbolChain[SymbolType]): Context =
      Context(
        constraint.mapSimpleConstraints {
          case SymbolContext(sym, _) if sym.matchesPrefix(symbol) => NoConstraints
          case s => s
        }
      )

    override def toString =
      constraint.prettyPrint()

//    def symbols: Traversable[(SymbolChain[SymbolType], Constraint)] =
//      new Context.ContextTraversable(this)

    def isEmpty = constraint == NoConstraints
  }

  case class SymbolContext(symbol: SymbolChain[SymbolType], constraint: Constraint) extends SimpleConstraint {

    override def &&(other: Constraint): Constraint = other match {
      case s: SymbolContext => tryAnd(s).getOrElse(And(List(this, s)))
      case s: SimpleConstraint => And(List(this, s))
      case And(cs) => And(this :: cs).simplify()
      case _ => other && this
    }

    override def ||(other: Constraint): Constraint = other match {
      case SymbolContext(sym, c) if sym == symbol =>
        copy(constraint = constraint || c)
      case s: SimpleConstraint => Or(List(And(this), And(s)))
      case a: And => Or(List(And(this), a))
      case Or(ands) => Or(And(this) :: ands)
    }

    override def unary_! : Constraint = ???

    override def tryAnd(other: SimpleConstraint): Option[SimpleConstraint] = other match {
      case SymbolContext(sym, con) if symbol == sym => constraint && con match {
        case NoConstraints => Some(NoConstraints)
        case ImpossibleConstraint => Some(ImpossibleConstraint)
        case c => Some(copy(constraint = c))
      }
      case _ => None
    }

    override def upperBound: Constraint = ???

    override def lowerBound: Constraint = ???

    override def isSymbolConstraint: Boolean = ???


    override def map[B](f: (ExpressionConstraint) => B)
        (implicit bf: ConstraintBuilder[B, ExpressionConstraint]): Constraint = ???
    override def flatMap(f: (ExpressionConstraint) => Constraint): Constraint = ???

    override def prettyPrint(x: String = "_") =
      s"(${symbol.prettyPrint} -> ${constraint.prettyPrint()})"
  }
}
