package mbergenlid.tools.boundedintegers

import scala.util.parsing.combinator.JavaTokenParsers
import mbergenlid.tools.boundedintegers.annotations.RichNumeric
import scala.reflect.runtime.universe._

trait ExpressionParser { self: Expressions =>

  class ExprParser[T: RichNumeric: TypeTag](symbols: List[RealSymbolType]) extends JavaTokenParsers {

    def parseExpression(input: String) =
      parseAll(expression, input)

    def expression: Parser[Expression] = term

    def term: Parser[Expression] =
      value ~ operator ~ value ^^ {case lhs ~ op ~ rhs => op(lhs, rhs)} |
      value

    def operator: Parser[(Expression, Expression) => Expression] =
      ("+" | "-" | "*") ^^ {
        case "+" => _ + _
        case "-" => _ - _
        case "*" => _ * _
      }

    def value: Parser[Expression] =
      (ident ^^ stringToExpression) |
        (wholeNumber ^^ {x: String => bigIntToExpression(BigDecimal(x))} )

    def bigIntToExpression(v: BigDecimal): Expression =
      Polynomial(Set(Term(TypedConstantValue[Int](v), Map.empty)))

    def stringToExpression(s: String): Expression =
      if(s == MethodExpressionFactory.ThisSymbol.name.toString) {
        Polynomial.fromSymbol[T](SymbolChain(List(MethodExpressionFactory.ThisSymbol)))
      } else
        Polynomial.fromSymbol[T](SymbolChain(List(symbols.find(_.name.toString == s).get)))
  }

}
