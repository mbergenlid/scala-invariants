package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

import scala.reflect.runtime.universe._

class BoundedTypeTreesSpec extends FunSuite 
    with BoundedTypeTrees {

  type RealSymbolType = Symbol
  val TypeNothing = typeOf[Nothing]
  val x = 0
  val y = 0

  var symbolCache = Map[String, SymbolType]()

  implicit def stringToSymbol(s: String): SymbolType = {
    if(!symbolCache.contains(s)) {
      symbolCache += (s -> SymbolChain(List(typeOf[this.type].termSymbol.
        newTermSymbol(newTermName(s), NoPosition, NoFlags | Flag.FINAL ))))
    }
    symbolCache(s)
  }

  implicit def bigIntToExpression(v: BigDecimal): Expression =
    Polynomial(Set(Term(TypedConstantValue[Int](v), Map.empty)))
  implicit def intToConstant(v: Int) = ConstantValue(v)
  implicit def intToExpresion(v: Int): Expression = Polynomial.fromConstant(v)

  def stringToExpression(s: String): Expression =
    Polynomial.fromSymbol[Int](stringToSymbol(s))

  implicit def c(s: String): Constraint =
    parser.parseExpression(s).get

  val parser = new ExprParser

  test("Test Basic Constant values") {
    val one = Polynomial.fromConstant(1)
    val five = Polynomial.fromConstant(5)
    val minusOne = Polynomial.fromConstant(-1)

    assert(one < five, s"$one should be < $five")
    assert(one <= five, s"$one should be <= $five")
    assert(one >= minusOne, s"$one should be >= $minusOne")
    assert(five > minusOne, s"$five should be > $minusOne")
    assert(five == Polynomial.fromConstant(5), s"$five should be == $five")
  }

  test("Test Basic SymbolExpression") {
    val x = Polynomial.fromSymbol[Int]("x")
    val y = Polynomial.fromSymbol[Int]("y")

    assert(!(x < y), s"!($x < $y)")
    assert(!(x < x), s"!($x < $x)")
    assert(x <= x, s"!($x <= $x)")
    assert(!(x <= y), s"!($x <= $y)")
  }

  /**
   * < 10 && ( > 0 && < y ) && < -5
   */
  test("Test simplification") {
    val and = c("x < 10 && (x > 0 && x < y)")

    val res = and && LessThan(15)
    assert(!res.exists(_ == LessThan(15)), res.prettyPrint())

    val res2 = and && LessThan(5)
    assert(res2.exists(_ == LessThan(5)), res2.prettyPrint())
    assert(!res2.exists(_ == LessThan(10)), res2.prettyPrint())

    val res3 = and && GreaterThan(-5)
    assert(!res3.exists(_ == GreaterThan(-5)), res3.prettyPrint())

    val res4 = and && GreaterThan(5)
    assert(!res4.exists(_ == GreaterThan(0)), res4.prettyPrint())
    assert(res4.exists(_ == GreaterThan(5)), res4.prettyPrint())
  }

  test("Simplification 2") {
    val and = c("x < 10 && x > 0")

    val res = and && c("x < 15 && x > 1")
    assert(res == c("x < 10 && x > 1"), res.prettyPrint())

    val res2 = c("x < 15 && x > 1") && and
    assert(res2 == c("x < 10 && x > 1"), res2.prettyPrint())

    val res3 = c("x < 10 && (x > 0 && x > y)") && c("x < 15 && x > 1")
    assert(res3 == c("x < 10 && (x > 1 && x > y)"), res3.prettyPrint())

    val res4 = c("(x < 10 && x > y) && x > 0") && c("x < 15 && x > 1")
    assert(res4 == c("(x < 10 && x > y) && x > 1"), res4.prettyPrint())

    val res5 = c("x > y && (x < 10 && x > 0)") && c("x < 15 && x > 1")
    assert(res5 == c("x > y && (x < 10 && x > 1)"), res5.prettyPrint())

//    assert("x < 2147483648".obviouslySubsetOf("x < 2147483648"))
//    val res6 = c("x == n && (x < 2147483648 && x > 0)") && c("x < 2147483648 && x >= -2147483647")
//    assert(res6 == c("x == n && (x < 2147483648 && x > 0)"), res6.prettyPrint())
  }


  /**
   * ((<10 && >0) || >20) && <15
   * ((<10 && >0) || >20) && >25
   */
  test("Simplification with Or") {
    val or = Or(
      And(LessThan(Polynomial.fromConstant(10)),
      GreaterThan(Polynomial.fromConstant(0))),
      GreaterThan(20))

    val res = or && LessThan(15)
    assert(!res.exists(_ == LessThan(15)), res.prettyPrint())
    assert(res.exists(_ == LessThan(10)), res.prettyPrint())

    val res2 = or && GreaterThan(25)
    assert(res2.exists(_ == GreaterThan(25)), res2.prettyPrint())
    assert(!res2.exists(_ == LessThan(10)), res2.prettyPrint())

    val res3 = or && LessThan(25)
    assert(res3 === c("(x < 10 && x > 0) || (x > 20 && x < 25)"))
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
  assertConstraint("x < 10 <!< x <= -2147483648")

  assertConstraint("x < -2147483648 <:< x <= 10")
  assertConstraint("x < 10 <:< x <= 2147483647")
  assertConstraint("x >= 10 <:< x > 0")

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
        case "&&" => And.apply
        case "||" => Or.apply
      }

    def binOp: Parser[Expression => Constraint] =
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

  def assertConstraint(expr: String) {
    val result = parser.parseConstraint(expr).get
    val (e1, _, f) = result

    test(expr) {
      assert(f(), e1)
    }
  }
}
