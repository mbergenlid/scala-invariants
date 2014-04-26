package mbergenlid.tools.boundedintegers

import scala.language.implicitConversions
import mbergenlid.tools.boundedintegers.annotations.RichNumeric
import scala.reflect.runtime.universe._

trait Expressions {
  type RealSymbolType <:scala.reflect.api.Symbols#SymbolApi
  type TypeType = scala.reflect.api.Types#TypeApi

  case class SymbolChain(symbols: List[RealSymbolType]) extends SymbolType {
    def head = symbols.head
    def tail = SymbolChain(symbols.tail)
    def isStable = head.asTerm.isVal ||
      (head.asTerm.isGetter && head.asTerm.accessed.asTerm.isVal)

    def map(f: RealSymbolType => RealSymbolType) = SymbolChain(symbols.map(f))

    def prettyPrint =
      symbols.reverse.map(_.name.toString).mkString(".")
  }

  trait SymbolType {
    def head: RealSymbolType
    def tail: SymbolType
    def isStable: Boolean
    def symbols: List[RealSymbolType]
    def map(f: RealSymbolType => RealSymbolType): SymbolType

    def prettyPrint: String
  }

  val TypeNothing: TypeType

  trait Expression {
    def isConstant: Boolean =
      terms.size == 1 && terms.headOption.map(_.variables.isEmpty).getOrElse(true)
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
    def substitute(symbol: SymbolType, expr: Expression): Expression

    def containsSymbols: Boolean
    def increment: Expression
    def decrement: Expression

    def extractSymbols: Set[SymbolType]
    def extractSymbol(symbol: SymbolType): Expression
    def tpe: TypeType = terms.headOption match {
      case Some(term) => term.tpe
      case None => TypeNothing
    }

    def isNaN =
      terms.exists(_.coeff.isNaN)
  }

  class ExpressionFactory[T: RichNumeric: TypeTag](val convertedType: TypeType) {
    def fromConstant(constant: T) = Polynomial.fromConstant(constant)
    def fromSymbol(symbol: SymbolType) = Polynomial.fromSymbol(symbol)
    def convertConstant[U: RichNumeric](constant: U): Expression =
      Polynomial.fromConstant(implicitly[RichNumeric[T]].fromType[U](constant))

    def convertConstant(constant: ConstantValue): Expression =
      new Polynomial(Set(Term(constant.convertTo[T], Map.empty)))

    def convertExpression(expr: Expression): Expression =
      Polynomial(expr.terms.map(t => t.copy(coeff = t.coeff.convertTo[T])))

    def fromParameter(param: String): Expression =
      throw new UnsupportedOperationException("Not a method")

    lazy val MaxValue = fromConstant(implicitly[RichNumeric[T]].maxValue)
    lazy val MinValue = fromConstant(implicitly[RichNumeric[T]].minValue)
  }

  class MethodExpressionFactory[T: RichNumeric: TypeTag](resultType: TypeType, params: List[RealSymbolType]) extends
    ExpressionFactory[T](resultType) {

    override def fromParameter(s: String) =
      Polynomial.fromSymbol(SymbolChain(List(params.find(_.name == newTermName(s)).get)))
  }

  object Polynomial {
    def apply(terms: Set[Term]) =
      new Polynomial(terms)

    def apply(terms: Term*) =
      new Polynomial(terms.toSet[Term])

    def fromConstant[T: RichNumeric :TypeTag](constant: T) =
      new Polynomial(Set(Term(ConstantValue(constant), Map.empty)))

    def fromSymbol[T: RichNumeric :TypeTag](symbol: SymbolType) = {
      new Polynomial(
        Set(Term(ConstantValue(implicitly[RichNumeric[T]].fromInt(1)),
        Map(symbol -> 1))))
    }

    def Zero = new Polynomial(Set.empty)
  }

  class Polynomial(val terms: Set[Term]) extends Expression  {
    def >(that: Expression ): Boolean =
      (!isNaN && !that.isNaN) && {
        val diff = (this - that).terms
        !diff.isEmpty && diff.forall(t => t.greaterThanZero)
      }

    def >=(that: Expression ): Boolean=
      (!isNaN && !that.isNaN) &&
        (this - that).terms.forall(t => t.greaterThanZero || t.isZero)

    def <(that: Expression ): Boolean = {
      (!isNaN && !that.isNaN) && {
        val diff = (this - that).terms
        !diff.isEmpty && diff.forall(t => t.lessThanZero)
      }
    }
    def <=(that: Expression ): Boolean = {
      (!isNaN && !that.isNaN) &&
        (this - that).terms.forall(t => t.lessThanZero || t.isZero)
    }

    def ==(that: Expression ): Boolean=
      (this - that).terms.forall(_.isZero)


    def +(that: Expression ): Expression  = Polynomial(
      (terms /: that.terms) {(set, term) =>
        addTerm(set, term) 
      }
    )

    private def addTerm(terms: Set[Term ], that: Term ): Set[Term ] = {
      terms.find(_.+.isDefinedAt(that)) match {
        case Some(t) => 
          val newTerm = t + that
          if(!newTerm.coeff.isNaN && newTerm.isZero) terms - t
          else (terms - t) + newTerm
        case None => terms + that
      }
    }

    def -(that: Expression ): Expression  = this + (-that)
    def *(that: Expression ): Expression  = Polynomial(for {
      thisTerm <- terms
      thatTerm <- that.terms
    } yield { thisTerm * thatTerm })

    def unary_- : Expression = map(_.unary_-)
    def substitute(symbol: SymbolType, expr: Expression): Expression =
      terms.foldLeft[Expression](Polynomial.Zero) {(p, t) => {
        p + t.substitute(symbol, expr) 
      }}

    def containsSymbols =
      terms.exists(_.variables != Map.empty)

    def increment =
      if(terms.size == 1) Polynomial(terms.map(_.increment))
      else this

    def decrement =
      if(terms.size == 1) Polynomial(terms.map(_.decrement))
      else this

    def extractSymbols: Set[SymbolType] = for {
      term <- terms
      (symbol, mult) <- term.variables
    } yield symbol

    def extractSymbol(symbol: SymbolType) = {
      terms.find(_.variables.contains(symbol)) match {
        case Some(term) =>
          new Polynomial(terms.collect {
            case t if t != term => -t
          })
        case None => this
      }
    }

    def map(f: Term  => Term ) =
      Polynomial(terms.map(f))

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
        expr * Polynomial(Set(Term(coeff, variables - symbol)))
      } else {
        Polynomial(Set(this))
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
        def printVariable(v: SymbolType, mult: Int) =
          v.prettyPrint + (
            if(mult == 1) ""
            else "^" + mult
          )
        variables.map(t => printVariable(t._1, t._2)).mkString("*")
      }

      if(variables.isEmpty) coeff.toString
      else if(coeff.isOne) printVariables
      else if(coeff == -coeff.one) "-" + printVariables
      else (coeff.toString + "*") + printVariables
    }

    def increment =
      if(variables.isEmpty) Term(coeff.increment, variables)
      else this

    def decrement =
      if(variables.isEmpty) Term(coeff.decrement, variables)
      else this

    def tpe: TypeType = coeff.tpe
  }

  case class SymbolExpression(symbol: SymbolType) {
    override def toString = symbol.toString
  }

  trait ConstantValue extends Ordered[ConstantValue] {
    type T
    implicit protected[boundedintegers] def typeInfo: TypeTag[T]
    implicit protected def num: RichNumeric[T]
    protected def value: T
    def tpe: TypeType = typeInfo.tpe

    def isNaN = false

    def convertTo[U: RichNumeric: TypeTag] =
      ConstantValue(implicitly[RichNumeric[U]].fromType[T](value))

    def zero = TypedConstantValue(num.zero)
    def one = TypedConstantValue(num.one)
    def isOne = value == num.one
    def isZero = value == num.zero
    def isGreaterThanZero = num.gt(value, num.zero)
    def isLessThanZero = num.lt(value, num.zero)

    def unary_- = ConstantValue(
      if(value == num.minValue) num.maxValue
      else if(value == num.maxValue) num.negate(value)
      else num.negate(value)
    )

    def +(that: ConstantValue) = withConcreteType(that) { thatValue =>
      if(num.gt(thatValue, num.zero) && num.gt(value, num.minus(num.maxValue, thatValue)))
        ConstantValue.overflow[T]
      else if(num.lt(thatValue, num.zero) && num.lt(value, num.minus(num.minValue, thatValue)))
        ConstantValue.underflow[T]
      else ConstantValue(num.plus(value, thatValue))
    }


    def *(that: ConstantValue): ConstantValue = withConcreteType(that) { thatValue =>
      ConstantValue(num.times(value, thatValue))
    }

    def increment =
      if(num.isInstanceOf[Integral[_]] && num.lt(value, num.maxValue))
        ConstantValue(num.plus(value, num.one))
      else
        this

    def decrement =
      if(num.isInstanceOf[Integral[_]] && num.gt(value, num.minValue))
        ConstantValue(num.minus(value, num.one))
      else
        this

    override def compare(other: ConstantValue) = withConcreteType(other) { v =>
      num.compare(value, v)
    }
    override def toString = value.toString
    def substitute(symbol: SymbolType, expr: Expression) = this

    private def withConcreteType[U](const: ConstantValue)(f: T => U): U = {
        assert(const.typeInfo == typeInfo, s"Type Mismatch in Expression: ${const.typeInfo} vs $typeInfo")
      val thatValue = const.value.asInstanceOf[T]
      f(thatValue)
    }
  }

  object ConstantValue {
    def apply[U: RichNumeric : TypeTag](value: U): ConstantValue =
      TypedConstantValue[U](value)

    def overflow[U: RichNumeric : TypeTag] =
      new OverflowConstant[U]

    def underflow[U: RichNumeric : TypeTag] =
      new UnderflowConstant[U]
  }

  class OverflowConstant[U](implicit val num: RichNumeric[U],
                            protected[boundedintegers] val typeInfo: TypeTag[U]) extends ConstantValue {
    override protected val value = implicitly[RichNumeric[T]].maxValue
    override type T = U
    override def toString = "Overflow"
    override def isNaN = true
    override def unary_- = this
  }

  class UnderflowConstant[U](implicit val num: RichNumeric[U],
                            protected[boundedintegers] val typeInfo: TypeTag[U]) extends ConstantValue {
    override protected val value = implicitly[RichNumeric[T]].minValue
    override type T = U
    override def toString = "Underflow"
    override def isNaN = true
    override def unary_- = this
  }

  case class TypedConstantValue[U](protected val value: U)
                                  (implicit ev1: RichNumeric[U],
                                   protected[boundedintegers] val typeInfo: TypeTag[U]) extends ConstantValue {
    type T = U
    override protected def num = implicitly[RichNumeric[U]]
  }

  implicit object ConstantValueOrdering extends Ordering[ConstantValue] {
    override def compare(x: ConstantValue, y: ConstantValue): Int = x.compare(y)
  }
}
