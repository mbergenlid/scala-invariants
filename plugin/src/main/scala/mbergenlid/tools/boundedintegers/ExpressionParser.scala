package mbergenlid.tools.boundedintegers

import mbergenlid.tools.boundedintegers.facades.TypeFacades

import scala.util.parsing.combinator.JavaTokenParsers
import mbergenlid.tools.boundedintegers.annotations.RichNumeric
import scala.reflect.runtime.universe._

trait ExpressionParser { self: Expressions =>

  class ExprParser[T: RichNumeric: TypeTag](symbols: List[RealSymbolType], typeFacades: TypeFacades) extends JavaTokenParsers {

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
      symbol |
      (ident ^^ stringToExpression) |
        (wholeNumber ^^ {x: String => bigIntToExpression(BigDecimal(x))} )


    def bigIntToExpression(v: BigDecimal): Expression =
      Polynomial(Set(Term(TypedConstantValue(v), Map.empty)))

    def symbol: Parser[Expression] =
      ident ~ "." ~ repsep(ident, ".") ^^ {case obj ~ "." ~ rest => listToExpression(obj :: rest)}

    def listToExpression(l: List[String]): Expression = {
      val firstSymbol: typeFacades.Symbol = symbols.find(_.name.toString == l.head).get.asInstanceOf[typeFacades.Symbol]
      val symbolChain = (List(firstSymbol) /: l.tail) { (symbols, name) =>
        val symbol: typeFacades.Symbol =
        symbols.head.typeSignature.
          members.find(_.name.toString == name).get

        typeFacades.findFacadeForSymbol(symbol) :: symbols
      }
      Polynomial.fromSymbol[T](SymbolChain(symbolChain.asInstanceOf[List[RealSymbolType]]))
    }

    def stringToExpression(s: String): Expression =
      if(s == MethodExpressionFactory.ThisSymbol.name.toString) {
        Polynomial.fromSymbol[T](SymbolChain(List(MethodExpressionFactory.ThisSymbol)))
      } else
        Polynomial.fromSymbol[T](SymbolChain(List(symbols.find(_.name.toString == s).get)))
  }

}
