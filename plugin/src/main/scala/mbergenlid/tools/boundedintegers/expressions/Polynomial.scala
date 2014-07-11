package mbergenlid.tools.boundedintegers.expressions

import scala.reflect.api.Symbols
import mbergenlid.tools.boundedintegers.SymbolChain
import mbergenlid.tools.boundedintegers.annotations.RichNumeric

class Polynomial[S <: Symbols#SymbolApi] private[expressions] (val terms: Set[Term[S]], isIntegral: Boolean) {
  type SymbolType = SymbolChain[S]

  def >(that: Polynomial[S]): Boolean = {
    val diff = (this - that).terms
    diff.nonEmpty && diff.forall(t => t.greaterThanZero)
  }

  def >=(that: Polynomial[S] ): Boolean=
    (this - that).terms.forall(t => t.greaterThanZero || t.isZero)

  def <(that: Polynomial[S] ): Boolean = {
    val diff = (this - that).terms
    diff.nonEmpty && diff.forall(t => t.lessThanZero)
  }
  def <=(that: Polynomial[S] ): Boolean = {
    (this - that).terms.forall(t => t.lessThanZero || t.isZero)
  }

  def ==(that: Polynomial[S] ): Boolean=
    (this - that).terms.forall(_.isZero)

  def isConstant: Boolean =
    terms.isEmpty ||
      (terms.size == 1 && terms.headOption.map(_.variables.isEmpty).getOrElse(true))

  def asConstant: ConstantValue = {
    assert(isConstant)
    terms.headOption.map(_.coeff).getOrElse(ConstantValue.apply(0L))
  }

  def +(that: Polynomial[S]): Polynomial[S]  = new Polynomial[S](
    (terms /: that.terms) {(set, term) =>
      addTerm(set, term)
    },
    isIntegral
  )

  private def addTerm(terms: Set[Term[S]], that: Term[S]): Set[Term[S]] = {
    terms.find(_.+.isDefinedAt(that)) match {
      case Some(t) =>
        val newTerm = t + that
        if(newTerm.isZero) terms - t
        else (terms - t) + newTerm
      case None => terms + that
    }
  }

  def -(that: Polynomial[S] ): Polynomial[S]  = this + (-that)
  def *(that: Polynomial[S] ): Polynomial[S]  = new Polynomial[S](for {
    thisTerm <- terms
    thatTerm <- that.terms
  } yield { thisTerm * thatTerm }, isIntegral)

  def unary_- : Polynomial[S] = map(_.unary_-)
  def substitute(symbol: SymbolType, expr: Polynomial[S]): Polynomial[S] = {
    terms.foldLeft(Polynomial.Zero[S]) {(p, t) => {
      p + t.substitute(symbol, expr, isIntegral)
    }}
  }

  def substituteConstant(expr: Polynomial[S]) = {
    var substituted = false
    val newExpr = terms.foldLeft(Polynomial.Zero[S]) {(p, t) => {
      p + (if(t.variables.isEmpty) {substituted = true; expr} else new Polynomial[S](Set(t), isIntegral))
    }}

    if(substituted) newExpr
    else newExpr + expr
  }

  def containsSymbols =
    terms.exists(_.variables != Map.empty)

  def increment =
    if(isIntegral) this + Polynomial.fromConstant[Int, S](1)
    else this

  def decrement =
    if(isIntegral) this - Polynomial.fromConstant[Int, S](1)
    else this

  def extractSymbols: Set[SymbolType] = for {
    term <- terms
    (symbol, mult) <- term.variables
  } yield symbol

  def extractSymbol(symbol: SymbolType) = {
    terms.find(_.variables.contains(symbol)) match {
      case Some(term) =>
        new Polynomial[S](terms.collect {
          case t if t != term => -t
        }, isIntegral)
      case None => this
    }
  }

  def map(f: Term[S] => Term[S]) =
    new Polynomial[S](terms.map(f), isIntegral)

  override def toString =
    if(terms.isEmpty) "0"
    else terms.mkString(" + ")

  implicit object ConstantValueOrdering extends Ordering[ConstantValue] {
    override def compare(x: ConstantValue, y: ConstantValue): Int = x.compare(y)

  }
}

object Polynomial {
  def apply[S <: Symbols#SymbolApi](terms: Set[Term[S]]) =
    new Polynomial(terms, true)

  def apply[S <: Symbols#SymbolApi](terms: Term[S]*) =
    new Polynomial(terms.toSet[Term[S]], true)

  def fromConstant[T: RichNumeric, S <: Symbols#SymbolApi](constant: T) =
    new Polynomial[S](Set(Term(ConstantValue(implicitly[RichNumeric[T]].toBigDecimal(constant)), Map.empty)), implicitly[RichNumeric[T]].isInstanceOf[Integral[_]])

  def fromSymbol[T: RichNumeric, S <: Symbols#SymbolApi](symbol: SymbolChain[S]) = {
    new Polynomial[S](
      Set(Term(ConstantValue(BigDecimal(1)),
        Map(symbol -> 1))),
      implicitly[RichNumeric[T]].isInstanceOf[Integral[_]])
  }

  def Zero[S <: Symbols#SymbolApi] = new Polynomial[S](Set.empty, true)
}

case class Term[S <: Symbols#SymbolApi](coeff: ConstantValue, variables: Map[SymbolChain[S], Int]) {
  type SymbolType = SymbolChain[S]

  def unary_- = Term(-coeff, variables)
  def isZero = coeff.isZero
  def greaterThanZero = variables.isEmpty && coeff.isGreaterThanZero
  def lessThanZero = variables.isEmpty && coeff.isLessThanZero

  def substitute(symbol: SymbolType, expr: Polynomial[S], isIntegral: Boolean): Polynomial[S]  =
    if(variables.contains(symbol)) {
      expr * new Polynomial[S](Set(Term(coeff, variables - symbol)), isIntegral)
    } else {
      new Polynomial[S](Set(this), isIntegral)
    }


  def + : PartialFunction[Term[S], Term[S]] = {
    case t if t.isZero => this
    case Term(c, s) if variables == s && isAllVals =>
      Term(coeff+c, variables)
  }

  def isAllVals =
    variables.forall(v => v._1.isStable)

  def *(that: Term[S]): Term[S]  = {
    val newCoeff = coeff*that.coeff
    if(newCoeff.isZero) Term[S](newCoeff, Map.empty)
    else Term[S](newCoeff, (variables /: that.variables) {
      (vars, v) =>
        val multiplicity = vars.getOrElse(v._1, 0)
        vars + (v._1 -> (v._2 + multiplicity))
    })
  }

  override def toString = {
    def printVariables = {
      def printVariable(v: SymbolType, mult: Int) =
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
  lazy val One = ConstantValue(1)
}
