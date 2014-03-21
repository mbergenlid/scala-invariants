package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions
import mbergenlid.tools.boundedintegers.annotations.RichNumeric

trait Expressions {
  type BoundedSymbol

  trait Expression[T] {
    def terms: Set[Term[T]]
    def >(that: Expression[T]): Boolean
    def >=(that: Expression[T]): Boolean
    def <(that: Expression[T]): Boolean
    def <=(that: Expression[T]): Boolean
    def ==(that: Expression[T]): Boolean


    override def equals(obj: scala.Any): Boolean = obj match {
      case e: Expression[T] => this == e
      case _ => super.equals(obj)
    }

    def +(that: Expression[T]): Expression[T]
    def -(that: Expression[T]): Expression[T]
    def *(that: Expression[T]): Expression[T]

    def unary_- : Expression[T]
    def substitute(symbol: BoundedSymbol, expr: Expression[T]): Expression[T]

    def containsSymbols: Boolean
    def increment: Expression[T]
    def decrement: Expression[T]

    def extractSymbols: Set[BoundedSymbol]
  }

  object Polynom {
    def apply[T: RichNumeric](terms: Set[Term[T]]) =
      new Polynom[T](terms)

    def apply[T: RichNumeric](terms: Term[T]*) =
      new Polynom[T](terms.toSet[Term[T]])

    def fromConstant[T: RichNumeric](constant: T) =
      new Polynom[T](Set(Term(ConstantValue(constant), Map.empty)))

    def fromSymbol[T: RichNumeric](symbol: BoundedSymbol) =
      new Polynom[T](
        Set(Term[T](ConstantValue(implicitly[RichNumeric[T]].fromInt(1)),
        Map(symbol -> 1))))

    def Zero[T: RichNumeric] = new Polynom[T](Set.empty)
  }

  class Polynom[T: RichNumeric](val terms: Set[Term[T]]) extends Expression[T] {
    def >(that: Expression[T]): Boolean = {
      val diff = (this - that).terms
      !diff.isEmpty && diff.forall(_.greaterThanZero)
    }
    def >=(that: Expression[T]): Boolean=
      (this - that).terms.forall(t => t.greaterThanZero || t.isZero)
    def <(that: Expression[T]): Boolean = {
      val diff = (this - that).terms
      !diff.isEmpty && diff.forall(_.lessThanZero)
    }
    def <=(that: Expression[T]): Boolean=
      (this - that).terms.forall(t => t.lessThanZero || t.isZero)
    def ==(that: Expression[T]): Boolean=
      (this - that).terms.forall(_.isZero)


    def +(that: Expression[T]): Expression[T] = Polynom(
      (terms /: that.terms) {(set, term) =>
        addTerm(set, term) 
      }
    )

    private def addTerm(terms: Set[Term[T]], that: Term[T]): Set[Term[T]] = {
      terms.find(_.+.isDefinedAt(that)) match {
        case Some(t) => 
          val newTerm = t + that
          if(newTerm.isZero) terms - t
          else (terms - t) + newTerm
        case None => terms + that
      }
    }

    def -(that: Expression[T]): Expression[T] = this + (-that)
    def *(that: Expression[T]): Expression[T] = Polynom(for {
      thisTerm <- terms
      thatTerm <- that.terms
    } yield { thisTerm * thatTerm })

    def unary_- : Expression[T] = map(_.unary_-)
    def substitute(symbol: BoundedSymbol, expr: Expression[T]): Expression[T] =
      terms.foldLeft[Expression[T]](Polynom.Zero) {(p, t) => {
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

    def extractSymbols: Set[BoundedSymbol] = for {
      term <- terms
      (symbol, mult) <- term.variables
    } yield symbol

    def map(f: Term[T] => Term[T]) =
      Polynom(terms.map(f))

    override def toString =
      if(terms.isEmpty) "0"
      else terms.mkString(" + ")
  }

  case class Term[T: RichNumeric](coeff: ConstantValue[T], variables: Map[BoundedSymbol, Int]) {
    def unary_- = Term(-coeff, variables)
    def isZero = coeff.isZero
    def greaterThanZero = variables.isEmpty && coeff.isGreaterThanZero
    def lessThanZero = variables.isEmpty && coeff.isLessThanZero

    def substitute(symbol: BoundedSymbol, expr: Expression[T]): Expression[T] =
      if(variables.contains(symbol)) {
        expr * Polynom(Set(Term[T](coeff, variables - symbol)))
      } else {
        Polynom(Set(this))
      }


    def + : PartialFunction[Term[T], Term[T]] = {
      case t if t.isZero => this
      case Term(c, s) if variables == s =>
        Term(coeff+c, variables)
    }

    def *(that: Term[T]): Term[T] = {
      val newCoeff = coeff*that.coeff
      if(newCoeff.isZero) Term(ConstantValue(that.coeff.num.zero), Map.empty)
      else Term[T](newCoeff, (variables /: that.variables) {
        (vars, v) =>
          val multiplicity = vars.getOrElse(v._1, 0)
          vars + (v._1 -> (v._2 + multiplicity))
      })
    }

    override def toString = {
      if(variables.isEmpty) coeff.toString
      else if(coeff.isOne) ("" /: variables) ((s, t) => s + t._1)
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

  case class ConstantValue[T: RichNumeric](value: T) {
    val num = implicitly[RichNumeric[T]]
    import num._

    def isOne = value == num.one
    def isZero = value == num.zero
    def isGreaterThanZero = num.gt(value, num.zero)
    def isLessThanZero = num.lt(value, num.zero)

    def unary_- = ConstantValue[T](
      if(value == num.minValue) num.maxValue
      else if(value == num.maxValue) num.minValue
      else -value
    )

    def +(that: ConstantValue[T]) =
      if(num.gt(that.value, num.zero) && num.gt(value, num.maxValue - that.value))
        ConstantValue[T](num.maxValue)
      else if(num.lt(that.value, num.zero) && num.lt(value, num.minValue - that.value))
        ConstantValue(num.minValue)
      else ConstantValue(value + that.value)

    def *(that: ConstantValue[T]): ConstantValue[T] = ConstantValue(value * that.value)

    def increment = if(num.isInstanceOf[Integral[_]]) ConstantValue(value + num.one) else this
    def decrement = if(num.isInstanceOf[Integral[_]]) ConstantValue(value - num.one) else this

    override def toString = value.toString
    def substitute(symbol: BoundedSymbol, expr: Expression[T]) = this
  }
}
