package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions
import mbergenlid.tools.boundedintegers.annotations.RichNumeric
import scala.reflect.runtime.universe._

trait Expressions {
  type SymbolType <: scala.reflect.api.Symbols#SymbolApi
  type TypeType = scala.reflect.api.Types#TypeApi

  val TypeNothing: TypeType

  trait Expression {
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
    def substitute(symbol: SymbolType, expr: Expression): Expression

    def containsSymbols: Boolean
    def increment: Expression
    def decrement: Expression

    def extractSymbols: Set[SymbolType]
    def typeInfo: TypeTag[_] = terms.headOption match {
      case Some(term) => term.coeff.typeInfo
      case None => typeTag[Nothing]
    }
  }

  class ExpressionFactory[T: RichNumeric: TypeTag](val convertedType: TypeType) {
    def fromConstant(constant: T) = Polynom.fromConstant(constant)
    def fromSymbol(symbol: SymbolType) = Polynom.fromSymbol(symbol)
    def convertConstant[U: RichNumeric](constant: U) =
      Polynom.fromConstant(implicitly[RichNumeric[T]].fromType[U](constant))

    def convertExpression(expr: Expression): Expression =
      Polynom(expr.terms.map(t => t.copy(coeff = t.coeff.convertTo[T])))

  }

  object Polynom {
    def apply(terms: Set[Term]) =
      new Polynom(terms)

    def apply(terms: Term*) =
      new Polynom(terms.toSet[Term])

    def fromConstant[T: RichNumeric :TypeTag](constant: T) =
      new Polynom(Set(Term(ConstantValue(constant), Map.empty)))

    def fromSymbol[T: RichNumeric :TypeTag](symbol: SymbolType) = {
      new Polynom(
        Set(Term(ConstantValue(implicitly[RichNumeric[T]].fromInt(1)),
        Map(symbol -> 1))))
    }

    def Zero = new Polynom(Set.empty)
  }

  class Polynom(val terms: Set[Term]) extends Expression  {
    def >(that: Expression ): Boolean = {
      val diff = (this - that).terms
      !diff.isEmpty && diff.forall(_.greaterThanZero)
    }
    def >=(that: Expression ): Boolean=
      (this - that).terms.forall(t => t.greaterThanZero || t.isZero)
    def <(that: Expression ): Boolean = {
      val diff = (this - that).terms
      !diff.isEmpty && diff.forall(_.lessThanZero)
    }
    def <=(that: Expression ): Boolean=
      (this - that).terms.forall(t => t.lessThanZero || t.isZero)
    def ==(that: Expression ): Boolean=
      (this - that).terms.forall(_.isZero)


    def +(that: Expression ): Expression  = Polynom(
      (terms /: that.terms) {(set, term) =>
        addTerm(set, term) 
      }
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
    def *(that: Expression ): Expression  = Polynom(for {
      thisTerm <- terms
      thatTerm <- that.terms
    } yield { thisTerm * thatTerm })

    def unary_- : Expression = map(_.unary_-)
    def substitute(symbol: SymbolType, expr: Expression): Expression =
      terms.foldLeft[Expression](Polynom.Zero) {(p, t) => {
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

    def extractSymbols: Set[SymbolType] = for {
      term <- terms
      (symbol, mult) <- term.variables
    } yield symbol

    def map(f: Term  => Term ) =
      Polynom(terms.map(f))

    override def toString =
      if(terms.isEmpty) "0"
      else terms.mkString(" + ")
  }

  case class Term(coeff: ConstantValue, variables: Map[SymbolType, Int]) {
    def unary_- = Term(-coeff, variables)
    def isZero = coeff.isZero
    def greaterThanZero = variables.isEmpty && coeff.isGreaterThanZero
    def lessThanZero = variables.isEmpty && coeff.isLessThanZero

    def substitute(symbol: SymbolType, expr: Expression ): Expression  =
      if(variables.contains(symbol)) {
        expr * Polynom(Set(Term(coeff, variables - symbol)))
      } else {
        Polynom(Set(this))
      }


    def + : PartialFunction[Term, Term] = {
      case t if t.isZero => this
      case Term(c, s) if variables == s =>
        Term(coeff+c, variables)
    }

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

  case class SymbolExpression(symbol: SymbolType) {
    override def toString = symbol.toString
  }

  trait ConstantValue {
    type T
    implicit protected[boundedintegers] def typeInfo: TypeTag[T]
    implicit protected def num: RichNumeric[T]
    protected def value: T

    def convertTo[U: RichNumeric: TypeTag] =
      ConstantValue(implicitly[RichNumeric[U]].fromType[T](value))

    def zero = TypedConstantValue(num.zero)
    def isOne = value == num.one
    def isZero = value == num.zero
    def isGreaterThanZero = num.gt(value, num.zero)
    def isLessThanZero = num.lt(value, num.zero)

    def unary_- = ConstantValue(
      if(value == num.minValue) num.maxValue
      else if(value == num.maxValue) num.minValue
      else num.negate(value)
    )

    def +(that: ConstantValue) = withConcreteType(that) { thatValue =>
      if(num.gt(thatValue, num.zero) && num.gt(value, num.minus(num.maxValue, thatValue)))
        ConstantValue(num.maxValue)
      else if(num.lt(thatValue, num.zero) && num.lt(value, num.minus(num.minValue, thatValue)))
        ConstantValue(num.minValue)
      else ConstantValue(num.plus(value, thatValue))
    }


    def *(that: ConstantValue): ConstantValue = withConcreteType(that) { thatValue =>
      ConstantValue(num.times(value, thatValue))
    }

    def increment = if(num.isInstanceOf[Integral[_]]) ConstantValue(num.plus(value, num.one)) else this
    def decrement = if(num.isInstanceOf[Integral[_]]) ConstantValue(num.minus(value, num.one)) else this

    override def toString = value.toString
    def substitute(symbol: SymbolType, expr: Expression) = this

    private def withConcreteType(const: ConstantValue)(f: T => ConstantValue): ConstantValue = {
        assert(const.typeInfo == typeInfo, s"Type Mismatch in Expression: ${const.typeInfo} vs $typeInfo")
      val thatValue = const.value.asInstanceOf[T]
      f(thatValue)
    }
  }

  object ConstantValue {
    def apply[U: RichNumeric : TypeTag](value: U): ConstantValue =
      TypedConstantValue[U](value)
  }

  case class TypedConstantValue[U](protected val value: U)
                                  (implicit ev1: RichNumeric[U],
                                   protected[boundedintegers] val typeInfo: TypeTag[U]) extends ConstantValue {
    type T = U
    override protected def num = implicitly[RichNumeric[U]]
  }
}
