package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions
import java.math.{BigDecimal => JBigDecimal}

trait Expressions {
  type BoundedSymbol

  implicit def constantToExpression(c: ConstantValue) =
    Polynom(Set(Term(c, Map.empty)))

  implicit def symbolToExpression(s: SymbolExpression) =
    Polynom(Set(Term(ConstantValue(1), Map(s.symbol -> 1))))

  implicit def int2BigDecimal(v: Int) = BigDecimal(v)

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

    def increment: Expression
    def decrement: Expression

    def unary_- : Expression

    def substitute(symbol: BoundedSymbol, expr: Expression): Expression
    def containsSymbols: Boolean
    def extractSymbols: Set[BoundedSymbol] = for {
      term <- terms
      (symbol, mult) <- term.variables
    } yield symbol

    def map(f: Term => Term) =
      Polynom(terms.map(f))

    override def toString =
      if(terms.isEmpty) "0"
      else terms.mkString(" + ")
  }

  case class Polynom(terms: Set[Term]) extends Expression {
    def >(that: Expression): Boolean = {
      val diff = (this - that).terms
      !diff.isEmpty && diff.forall(_.greaterThanZero)
    }
    def >=(that: Expression): Boolean=
      (this - that).terms.forall(t => t.greaterThanZero || t.isZero)
    def <(that: Expression): Boolean = {
      val diff = (this - that).terms
      !diff.isEmpty && diff.forall(_.lessThanZero)
    }
    def <=(that: Expression): Boolean=
      (this - that).terms.forall(t => t.lessThanZero || t.isZero)
    def ==(that: Expression): Boolean=
      (this - that).terms.forall(_.isZero)


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
    def substitute(symbol: BoundedSymbol, expr: Expression): Expression =
      terms.foldLeft[Expression](Polynom(Set.empty)) {(p, t) => {
        p + t.substitute(symbol, expr) 
      }}

    def containsSymbols =
      terms.exists(_.variables != Map.empty)



    def increment =
      if(terms.size == 1) Polynom(terms.map(_.increment))
      else this

    def decrement =
      if(terms.size == 1) Polynom(terms.map(_.decrement))
      else this
  }

  case class Term(coeff: ConstantValue, variables: Map[BoundedSymbol, Int]) {
    def unary_- = Term(-coeff, variables)
    def isZero = coeff.isZero
    def greaterThanZero = variables.isEmpty && coeff.expr > 0
    def lessThanZero = variables.isEmpty && coeff.expr < 0

    def substitute(symbol: BoundedSymbol, expr: Expression): Expression = 
      if(variables.contains(symbol)) {
        expr * Polynom(Set(Term(coeff, variables - symbol)))
      } else {
        Polynom(Set(this))
      }


    def + : PartialFunction[Term, Term] = {
      case Term(ConstantValue(JBigDecimal.ZERO, _), s) => this
      case Term(c, s) if variables == s =>
        Term(coeff+c, variables)
    }

    def *(that: Term): Term = {
      val Term(ConstantValue(v, _), s) = that
      val newCoeff = coeff.expr*v
      if(newCoeff == 0) Term(ConstantValue(0, coeff.isInteger), Map.empty)
      else Term(ConstantValue(newCoeff), (variables /: s) {
        (vars, v) =>
          val multiplicity = vars.getOrElse(v._1, 0)
          vars + (v._1 -> (v._2 + multiplicity))
      })
    }

    override def toString = {
      if(variables.isEmpty) coeff.toString
      else if(coeff.expr == JBigDecimal.ONE) ("" /: variables) ((s, t) => s + t._1)
      else ((coeff.toString + "*") /: variables) ((s, t) => s + t._1)
    }

    def increment =
      if(variables.isEmpty) Term(coeff.increment, variables)
      else this

    def decrement =
      if(variables.isEmpty) Term(coeff.decrement, variables)
      else this
  }

  case class SymbolExpression(symbol: BoundedSymbol) {
    override def toString = symbol.toString
  }

  case class ConstantValue(expr: BigDecimal, isInteger: Boolean = true) {

    def isZero = expr == 0
    def unary_- = copy(expr = -expr)

    def +(that: ConstantValue) = copy(expr = expr + that.expr)

    def increment = if(isInteger) ConstantValue(expr+1) else this
    def decrement = if(isInteger) ConstantValue(expr-1) else this

    override def toString = expr.toString
    def substitute(symbol: BoundedSymbol, expr: Expression) = this
  }

}
