package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions

trait Expressions {
  type BoundedSymbol

  implicit def constantToExpression(c: ConstantValue) =
    Polynom(Set(Term(c, Map.empty)))

  implicit def symbolToExpression(s: SymbolExpression) =
    Polynom(Set(Term(ConstantValue(1), Map(s -> 1))))

  trait Expression {
    def terms: Set[Term]
    def >(that: Expression): Boolean
    def >=(that: Expression): Boolean 
    def <(that: Expression): Boolean
    def <=(that: Expression): Boolean 
    def ==(that: Expression): Boolean 

    def +(that: Expression): Expression
    def -(that: Expression): Expression
    def *(that: Expression): Expression

    def increment: Expression = this
    def decrement: Expression = this

    def depth: Int = 1
    def unary_- : Expression

    def substitute(symbol: BoundedSymbol, expr: Expression): Expression
    def containsSymbols: Boolean

    def map(f: Term => Term) =
      Polynom(terms.map(f))

  }

  case class Polynom(terms: Set[Term]) extends Expression {
    def >(that: Expression): Boolean = false 
    def >=(that: Expression): Boolean= false 
    def <(that: Expression): Boolean = false 
    def <=(that: Expression): Boolean= false 
    def ==(that: Expression): Boolean= false

    def +(that: Expression): Expression = Polynom(
      (terms /: that.terms) {(set, term) =>
        addTerm(set, term) 
      }
    )

    private def addTerm(terms: Set[Term], that: Term) = {
      terms.find(_.+.isDefinedAt(that)) match {
        case Some(t) => 
          val newTerm = t + that
          if(newTerm.isZero) terms - t
          else (terms - t) + newTerm
        case None => terms + that
      }
    }

    def -(that: Expression): Expression = this + (-that)
    def *(that: Expression): Expression = Polynom(for {
      thisTerm <- terms
      thatTerm <- that.terms
    } yield { thisTerm * thatTerm })

    def unary_- : Expression = map(_.unary_-)
    def substitute(symbol: BoundedSymbol, expr: Expression): Expression = this
    def containsSymbols = throw new Exception

    override def toString = ("" /: terms) ((s,t) => s + s" ${t.toString}")
  }

  case class Term(coeff: ConstantValue, variables: Map[SymbolExpression, Int]) {
    def unary_- = Term(-coeff, variables)
    def isZero = coeff.expr == 0

    def >(that: Expression): Boolean = false 
    def >=(that: Expression): Boolean= false 
    def <(that: Expression): Boolean = false 
    def <=(that: Expression): Boolean= false 

    def substitute(symbol: BoundedSymbol, expr: Expression) = this

    def + : PartialFunction[Term, Term] = {
      case Term(ConstantValue(0), s) => this
      case Term(ConstantValue(v), s) if(variables == s) =>
        Term(ConstantValue(v+coeff.expr), variables)
    }

    def *(that: Term): Term = {
      val Term(ConstantValue(v), s) = that
      val newCoeff = coeff.expr*v
      if(newCoeff == 0) Term(ConstantValue(0), Map.empty)
      else Term(ConstantValue(newCoeff), (
        (variables /: s) { (vars, v) =>
          val multiplicity = vars.getOrElse(v._1, 0)
          vars + (v._1 -> (v._2 + multiplicity))
        }
      ))
    }

    override def toString = {
      val sign = if(coeff.expr >= 0) "+"
        else ""
      val s1 = if(variables.isEmpty) coeff.toString
      else (coeff.toString /: variables) ((s, t) => s + t)
      sign + s1
    }
  }

  case class SymbolExpression(symbol: BoundedSymbol) {
    def >(that: Expression) = false
    def >=(that: Expression) = this == that
    def <(that: Expression) = false
    def <=(that: Expression) = this == that
    def ==(that: Expression) = false


    override def toString = symbol.toString
  }

  case class ConstantValue(expr: Int) {
    def >(that: Expression) = compareWith(that, _>_)
    def >=(that: Expression) = compareWith(that, _>=_)
    def <(that: Expression) = compareWith(that, _<_)
    def <=(that: Expression) = compareWith(that, _<=_)
    def ==(that: Expression) = compareWith(that, _==_)
    def compareWith(other: Expression, op: Function2[Int, Int, Boolean]) = false
    def unary_- = ConstantValue(-expr)

    def increment = ConstantValue(expr+1)
    def decrement = ConstantValue(expr-1)

    override def toString = expr.toString
    def substitute(symbol: BoundedSymbol, expr: Expression) = this
  }
}
