package mbergenlid.scalainvariants.api.expressions

import mbergenlid.scalainvariants.annotations.RichNumeric
import scala.util.parsing.combinator.JavaTokenParsers
import mbergenlid.scalainvariants.api.{ApiUniverse, TypeFacades, SymbolChain}

trait ExpressionParsers {
  self: ApiUniverse =>
  
  class ExpressionParser[T: RichNumeric](
      symbols: List[SymbolType],
      typeFacades: TypeFacades[SymbolType])
    extends JavaTokenParsers {

    def parseExpression(input: String) =
      parseAll(expression, input)

    def expression: Parser[Expression] = term

    def term: Parser[Expression] =
      value ~ operator ~ value ^^ {case lhs ~ op ~ rhs => op(lhs, rhs)} |
        value

    def operator: Parser[(Expression, Expression) => Expression] =
      ("+" | "-" | "*" | "/") ^^ {
        case "+" => _ + _
        case "-" => _ - _
        case "*" => _ * _
        case "/" => _ / _
      }

    def value: Parser[Expression] =
      symbol |
        (ident ^^ stringToExpression) |
        (wholeNumber ^^ {x: String => bigIntToExpression(BigDecimal(x))} )


    def bigIntToExpression(v: BigDecimal): Expression =
      Polynomial(Set(Term(ConstantValue(v), Map.empty)))

    def symbol: Parser[Expression] =
      ident ~ "." ~ repsep(ident, ".") ^^ {case obj ~ "." ~ rest => listToExpression(obj :: rest)}

    def listToExpression(l: List[String]): Expression = {
      val firstSymbol: SymbolType = symbols.find(_.name.toString == l.head).get
      val symbolChain: List[SymbolType] = (List(firstSymbol) /: l.tail) { (symbols, name) =>
        val symbol: SymbolType =
          symbols.head.typeSignature.
            members.find(_.name.toString == name).get.asInstanceOf[SymbolType]

        typeFacades.findFacadeForSymbol(symbol) :: symbols
      }
      Polynomial.fromSymbol[T](SymbolChain(symbolChain))
    }

    def stringToExpression(s: String): Expression =
      symbols.find(_.name.toString == s).map { sym =>
        Polynomial.fromSymbol[T](SymbolChain(List(sym)))
      }.getOrElse(throw new NoSuchElementException(s"$s is not in $symbols"))
  }
}

