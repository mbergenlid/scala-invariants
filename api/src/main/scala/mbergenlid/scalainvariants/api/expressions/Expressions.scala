package mbergenlid.scalainvariants.api.expressions

import mbergenlid.scalainvariants.api.{SymbolChain, ApiUniverse}
import mbergenlid.tools.boundedintegers.annotations.RichNumeric

trait Expressions {
  self: ApiUniverse =>
  trait Expression {
    def isConstant: Boolean =
      terms.isEmpty ||
        (terms.size == 1 && terms.headOption.map(_.variables.isEmpty).getOrElse(true))

    def asConstant: ConstantValue = {
      assert(isConstant)
      terms.headOption.map(_.coeff).getOrElse(ConstantValue.apply(0L))
    }
    def terms: Set[Term]
    def >(that: Expression): Boolean
    def >=(that: Expression): Boolean
    def <(that: Expression): Boolean
    def <=(that: Expression): Boolean
    def ==(that: Expression): Boolean


    override def equals(obj: scala.Any): Boolean = obj match {
      case e: Expression => this == e
      case _ => super.equals(obj)
    }

    def +(that: Expression): Expression
    def -(that: Expression): Expression
    def *(that: Expression): Expression

    def unary_- : Expression
    def substitute(symbol: SymbolChain[SymbolType], expr: Expression): Expression
    def substituteConstant(expr: Expression): Expression

    def containsSymbols: Boolean
    def increment: Expression
    def decrement: Expression

    def extractSymbols: Set[SymbolChain[SymbolType]]
    def extractSymbol(symbol: SymbolChain[SymbolType]): Expression

    def map(f: Term  => Term): Expression
  }

  object Polynomial {
    def apply(terms: Set[Term]) =
      new Polynomial(terms, true)

    def apply(terms: Term*) =
      new Polynomial(terms.toSet[Term], true)

    def fromConstant[T: RichNumeric](constant: T) =
      new Polynomial(Set(Term(ConstantValue(constant), Map.empty)), implicitly[RichNumeric[T]].isInstanceOf[Integral[_]])

    def fromSymbol[T: RichNumeric](symbol: SymbolChain[SymbolType]) = {
      new Polynomial(
        Set(Term(ConstantValue(implicitly[RichNumeric[T]].fromInt(1)),
          Map(symbol -> 1))), implicitly[RichNumeric[T]].isInstanceOf[Integral[_]])
    }

    val Zero = new Polynomial(Set.empty, true)
  }

  class Polynomial private[expressions] (val terms: Set[Term], isIntegral: Boolean) extends Expression  {
    def >(that: Expression ): Boolean = {
      val diff = (this - that).terms
      diff.nonEmpty && diff.forall(t => t.greaterThanZero)
    }

    def >=(that: Expression ): Boolean=
      (this - that).terms.forall(t => t.greaterThanZero || t.isZero)

    def <(that: Expression ): Boolean = {
      val diff = (this - that).terms
      diff.nonEmpty && diff.forall(t => t.lessThanZero)
    }
    def <=(that: Expression ): Boolean = {
      (this - that).terms.forall(t => t.lessThanZero || t.isZero)
    }

    def ==(that: Expression ): Boolean=
      (this - that).terms.forall(_.isZero)


    def +(that: Expression ): Expression  = new Polynomial(
      (terms /: that.terms) {(set, term) =>
        addTerm(set, term)
      },
      isIntegral
    )

    private def addTerm(terms: Set[Term ], that: Term ): Set[Term ] = {
      terms.find(_.+.isDefinedAt(that)) match {
        case Some(t) =>
          val newTerm = t + that
          if(newTerm.isZero) terms - t
          else (terms - t) + newTerm
        case None => terms + that
      }
    }

    def -(that: Expression ): Expression  = this + (-that)
    def *(that: Expression ): Expression  = new Polynomial(for {
      thisTerm <- terms
      thatTerm <- that.terms
    } yield { thisTerm * thatTerm }, isIntegral)

    def unary_- : Expression = map(_.unary_-)
    def substitute(symbol: SymbolChain[SymbolType], expr: Expression): Expression = {
      terms.foldLeft[Expression](Polynomial.Zero) {(p, t) => {
        p + t.substitute(symbol, expr, isIntegral)
      }}
    }

    def substituteConstant(expr: Expression) = {
      var substituted = false
      val newExpr = terms.foldLeft[Expression](Polynomial.Zero) {(p, t) => {
        p + (if(t.variables.isEmpty) {substituted = true; expr} else new Polynomial(Set(t), isIntegral))
      }}

      if(substituted) newExpr
      else newExpr + expr
    }

    def containsSymbols =
      terms.exists(_.variables != Map.empty)

    def increment =
      if(isIntegral) this + Polynomial.fromConstant(1)
      else this

    def decrement =
      if(isIntegral) this - Polynomial.fromConstant(1)
      else this

    def extractSymbols: Set[SymbolChain[SymbolType]] = for {
      term <- terms
      (symbol, mult) <- term.variables
    } yield symbol

    def extractSymbol(symbol: SymbolChain[SymbolType]) = {
      terms.find(_.variables.contains(symbol)) match {
        case Some(term) =>
          new Polynomial(terms.collect {
            case t if t != term => -t
          }, isIntegral)
        case None => this
      }
    }

    def map(f: Term  => Term) =
      new Polynomial(terms.map(f), isIntegral)

    override def toString =
      if(terms.isEmpty) "0"
      else terms.mkString(" + ")

  }

  case class Term(coeff: ConstantValue, variables: Map[SymbolChain[SymbolType], Int]) {
    def unary_- = Term(-coeff, variables)
    def isZero = coeff.isZero
    def greaterThanZero = variables.isEmpty && coeff.isGreaterThanZero
    def lessThanZero = variables.isEmpty && coeff.isLessThanZero

    def substitute(symbol: SymbolChain[SymbolType], expr: Expression, isIntegral: Boolean): Expression  =
      if(variables.contains(symbol)) {
        expr * new Polynomial(Set(Term(coeff, variables - symbol)), isIntegral)
      } else {
        new Polynomial(Set(this), isIntegral)
      }


    def + : PartialFunction[Term, Term] = {
      case t if t.isZero => this
      case Term(c, s) if variables == s && isAllVals =>
        Term(coeff+c, variables)
    }

    def isAllVals =
      variables.forall(v => v._1.isStable)

    def *(that: Term ): Term  = {
      val newCoeff = coeff*that.coeff
      if(newCoeff.isZero) Term(newCoeff, Map.empty)
      else Term (newCoeff, (variables /: that.variables) {
        (vars, v) =>
          val multiplicity = vars.getOrElse(v._1, 0)
          vars + (v._1 -> (v._2 + multiplicity))
      })
    }

    override def toString = {
      def printVariables = {
        def printVariable(v: SymbolChain[SymbolType], mult: Int) =
          v.prettyPrint + (
            if(mult == 1) ""
            else "^" + mult
            )
        variables.map(t => printVariable(t._1, t._2)).mkString("*")
      }

      if(variables.isEmpty) coeff.toString
      else if(coeff.isOne) printVariables
      else if(coeff == -ConstantValue.One) "-" + printVariables
      else (coeff.toString + "*") + printVariables
    }

  }

  case class ConstantValue(value: BigDecimal) extends Ordered[ConstantValue] {

    def isOne = value == BigDecimal(1)
    def isZero = value == BigDecimal(0)
    def isGreaterThanZero = value > 0
    def isLessThanZero = value < 0

    def unary_- : ConstantValue = ConstantValue(-value)

    def +(that: ConstantValue) = {
      val newValue = value + that.value
      ConstantValue(newValue)
    }

    def *(that: ConstantValue): ConstantValue = {
      val newValue = value*that.value
      ConstantValue(newValue)
    }

    override def compare(other: ConstantValue) =
      value.compare(other.value)

    override def toString = value.toString()
  }

  object ConstantValue {
    def apply[U: RichNumeric](value: U): ConstantValue =
      ConstantValue(implicitly[RichNumeric[U]].toBigDecimal(value))

    lazy val One = ConstantValue(1)
  }
}


