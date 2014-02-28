package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

class BoundedTypeTreesSpec extends FunSuite 
    with BoundedTypeTrees {

  type BoundedSymbol = String

  implicit def intToConstant(v: Int) = ConstantValue(v)
  implicit def stringToSymbol(s: String) = SymbolExpression(s)


  val parser = new ExprParser

  test("Test Basic Constant values") {
    val one = ConstantValue(1)
    val five = ConstantValue(5)
    val minusOne = ConstantValue(-1)

    assert(one < five, s"$one should be < $five")
    assert(one <= five, s"$one should be <= $five")
    assert(one >= minusOne, s"$one should be >= $minusOne")
    assert(five > minusOne, s"$five should be > $minusOne")
    assert(five == ConstantValue(5), s"$five should be == $five")
  }

  test("Test Basic SymbolExpression") {
    val x = SymbolExpression("x")
    val y = SymbolExpression("y")

    assert(!(x < y), s"!($x < $y)")
    assert(!(x < x), s"!($x < $x)")
    assert((x <= x), s"!($x <= $x)")
    assert(!(x <= y), s"!($x <= $y)")
  }
  
  assertConstraint("x > 0 <!< x <= 10")
  assertConstraint("x >= 1 <:< x > 0")
  assertConstraint("x < 11 <:< x <= 10")
  assertConstraint("x > 2 <:< x >= 3")

  assertConstraint("x > 0 && x < 10 && x < y && x <= 99 <:< x > 0 && x < 100")

  assertConstraint("x >= 101 <:< x < 0 || x > 100")
  assertConstraint("x <= -1 || x >= 101 <:< x < 0 || x > 100")
  assertConstraint("x <= -1 || x >= 101 || x < 6 <!< x < 0 || x > 100")

  assertConstraint("x > 1 && x <= 10 <:< (x > 0 && x < 100) || (x > -100 && x < -10)")

  assertConstraint("x <= 10 && x >= 0 <:< x >=0 && x <= 10")
  assertConstraint("!(x > 10 || x < 0) <:< x >=0 && x <= 10")

  assertConstraint("x == 9 <:< x < 10")
  assertConstraint("x == 4 <:< x >= 0 && x <= 10")
  assertConstraint("x > 0 && x < maxValue <:< x >= 0")

  class ExprParser extends JavaTokenParsers {

    def parseConstraint(input: String) = 
      parseAll(constraint, input)

    def parseExpression(input: String) =
      parseAll(expr, input)

    def constraint: Parser[(Constraint, Constraint, () => Boolean)] =
      expr ~ "<!<" ~ expr ^^ { case e1 ~ op ~ e2 => (e1, e2, () => !(e1 obviouslySubsetOf e2)) } |
      expr ~ "<:<" ~ expr ^^ { case e1 ~ op ~ e2 => (e1, e2, () => e1 obviouslySubsetOf e2) }

    def expr: Parser[Constraint] =
      simpleConstraint ~ boolOp ~ expr ^^ {case c ~ op ~ expr => op(c, expr)} |
      simpleConstraint ^^ {case c => c}

    def simpleConstraint: Parser[Constraint] =
      "x" ~> binOp ~ value ^^ {case op ~ v => op(v)} |
      "(" ~> expr <~ ")" |
      "!(" ~> expr <~ ")" ^^ {case e => !e}
      
    def boolOp: Parser[(Constraint, Constraint) => Constraint] =
      ("&&" | "||" ) ^^ {
        case "&&" => And.apply _
        case "||" => Or.apply _
      }

    def binOp: Parser[Expression => Constraint] = 
      (">=" | "<=" | "<" | ">" | "==") ^^ {
        case ">" => GreaterThan.apply _
        case "<" => LessThan.apply _
        case "<=" => LessThanOrEqual.apply _
        case ">=" => GreaterThanOrEqual.apply _
        case "==" => Equal.apply _
      }


    def value: Parser[Expression] =
      (ident ^^ polynom) | (wholeNumber ^^ {x: String => polynom(x.toInt)} )

    def polynom(s: String) =
      Polynom(Set(Term(ConstantValue(1), Map(SymbolExpression(s) -> 1))))

    def polynom(c: Int) =
      Polynom(Set(Term(ConstantValue(c), Map.empty)))
  }

  def assertConstraint(expr: String) {
    val result = parser.parseConstraint(expr).get
    val (e1, e2, f) = result

    test(expr) {
      assert(f(), e1)
    }
  }
}
