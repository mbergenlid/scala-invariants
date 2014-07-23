package mbergenlid.scalainvariants.api.expressions

import mbergenlid.tools.boundedintegers.annotations.RichNumeric
import mbergenlid.scalainvariants.api.{ApiUniverse, TypeFacades, SymbolChain}
import scala.reflect.api.Symbols

trait ExpressionFactories {
  self: ApiUniverse =>

  class ExpressionFactory[T: RichNumeric](
    facades: TypeFacades,
    extraContext: List[Symbols#SymbolApi] = Nil) {

    val numeric = implicitly[RichNumeric[T]]
    def fromConstant(constant: T) = Polynomial.fromConstant(constant)
    def fromSymbol(symbol: SymbolChain) = Polynomial.fromSymbol(symbol)
    def convertConstant[U: RichNumeric](constant: U): Expression =
      Polynomial.fromConstant(numeric.fromType[U](constant))

    def convertConstant(constant: ConstantValue): Expression =
      new Polynomial(Set(Term(constant, Map.empty)), numeric.isInstanceOf[Integral[_]])

    def convertExpression(expr: Expression): Expression =
      new Polynomial(expr.terms, numeric.isInstanceOf[Integral[_]])

    def fromParameter(param: String): Expression =
      new ExpressionParser[T](extraContext, facades).parseExpression(param).get

    def withExtraContext(symbols: List[Symbols#SymbolApi]) =
      new ExpressionFactory(facades, extraContext ++ symbols)

    lazy val MaxValue = fromConstant(implicitly[RichNumeric[T]].maxValue)
    lazy val MinValue = fromConstant(implicitly[RichNumeric[T]].minValue)
  }
}
