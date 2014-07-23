package mbergenlid.scalainvariants.api.util


import scala.util.parsing.combinator.JavaTokenParsers
import scala.reflect.runtime.universe._
import scala.language.implicitConversions
import mbergenlid.scalainvariants.api.{ApiUniverse, SymbolChain}

trait TestExpressionParser {
  self: TestUniverse =>

  implicit def c(s: String): Constraint =
    parser.parseExpression(s).get
  val parser = new ExprParser

  implicit def bigIntToExpression(v: BigDecimal): Expression =
    Polynomial(Set(Term(ConstantValue(v), Map.empty)))
  implicit def intToConstant(v: Int) = ConstantValue(v)
  implicit def intToExpresion(v: Int): Expression = Polynomial.fromConstant(v)

  def stringToExpression(s: String): Expression =
    Polynomial.fromSymbol[Int](stringToSymbol(s))

  var symbolCache = Map[String, SymbolChain[SymbolType]]()

  implicit def stringToSymbol(s: String): SymbolChain[SymbolType] = {
    if(!symbolCache.contains(s)) {
      symbolCache += (s -> SymbolChain[SymbolType](List(typeOf[this.type].termSymbol.
        newTermSymbol(newTermName(s), NoPosition, NoFlags | Flag.FINAL ))))
    }
    symbolCache(s)
  }

  def ec(s: String): ExpressionConstraint =
    c(s).asInstanceOf[ExpressionConstraint]

  class ExprParser extends JavaTokenParsers {

    def parseConstraint(input: String) =
      parseAll(constraint, input)

    def parseExpression(input: String) =
      parseAll(expr, input)

    def constraint: Parser[(Constraint, Constraint, () => Boolean)] =
      expr ~ "<!<" ~ expr ^^ { case e1 ~ op ~ e2 => (e1, e2, () => !(e1 definitelySubsetOf e2)) } |
        expr ~ "<:<" ~ expr ^^ { case e1 ~ op ~ e2 => (e1, e2, () => e1 definitelySubsetOf e2) }

    def expr: Parser[Constraint] =
      simpleConstraint ~ boolOp ~ expr ^^ {case c ~ op ~ expr => op(c, expr)} |
        simpleConstraint ^^ {case c => c}

    def simpleConstraint: Parser[Constraint] =
      "x" ~> binOp ~ value ^^ {case op ~ v => op(v)} |
        "(" ~> expr <~ ")" |
        "!(" ~> expr <~ ")" ^^ {case e => !e}

    def boolOp: Parser[(Constraint, Constraint) => Constraint] =
      ("&&" | "||" ) ^^ {
        case "&&" => _&&_
        case "||" => _||_
      }

    def binOp: Parser[Expression => ExpressionConstraint] =
      (">=" | "<=" | "<" | ">" | "==") ^^ {
        case ">" => GreaterThan
        case "<" => LessThan
        case "<=" => LessThanOrEqual
        case ">=" => GreaterThanOrEqual
        case "==" => Equal
      }


    def value: Parser[Expression] =
      (ident ^^ stringToExpression) | (wholeNumber ^^ {x: String => bigIntToExpression(BigDecimal(x))} )

  }
}
