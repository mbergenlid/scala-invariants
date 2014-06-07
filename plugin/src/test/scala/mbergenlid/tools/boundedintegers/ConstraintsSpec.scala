package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.reflect.runtime.universe._
import scala.util.parsing.combinator.JavaTokenParsers

import scala.language.implicitConversions

class ConstraintsSpec extends FunSuite
   with Constraints {

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

  def ec(s: String): ExpressionConstraint =
    c(s).asInstanceOf[ExpressionConstraint]

  implicit def c(s: String): Constraint =
    parser.parseExpression(s).get
  val parser = new ExprParser

  test("ExpressionConstraint.&&") {
    val res1 = LessThanOrEqual(1) && GreaterThanOrEqual(1)
    assert(res1 === Equal(1))
  }

  test("Test simplification") {
    val and = LessThan(10) && (GreaterThan(0) && LessThan(stringToExpression("y")))

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

    val res6 = and && And(GreaterThan(15))
    assert(res6 === And(ImpossibleConstraint))
    //    assert("x < 2147483648".obviouslySubsetOf("x < 2147483648"))
//    val res6 = c("x == n && (x < 2147483648 && x > 0)") && c("x < 2147483648 && x >= -2147483647")
//    assert(res6 == c("x == n && (x < 2147483648 && x > 0)"), res6.prettyPrint())
  }

  /**
   * ((<10 && >0) || >20) && <15
   * ((<10 && >0) || >20) && >25
   */
  test("Simplification with Or") {
    val or = Or(List(
      And(LessThan(10), GreaterThan(0)),
      And(GreaterThan(20))
    ))

    val res = or && LessThan(15)
    assert(!res.exists(_ == LessThan(15)), res.prettyPrint())
    assert(res.exists(_ == LessThan(10)), res.prettyPrint())
    assert(!res.exists(_ == GreaterThan(20)), res.prettyPrint())
    assert(res === c("x < 10 && x > 0"))

    val res2 = or && GreaterThan(25)
    assert(res2.exists(_ == GreaterThan(25)), res2.prettyPrint())
    assert(!res2.exists(_ == LessThan(10)), res2.prettyPrint())
    val res22 = GreaterThan(25) && or
    assert(res22.exists(_ == GreaterThan(25)), res22.prettyPrint())
    assert(!res22.exists(_ == LessThan(10)), res22.prettyPrint())

    //((<10 && >0) || >20) && <25
    val res3 = or && LessThan(25)
    assert(res3 === c("(x < 10 && x > 0) || (x < 25 && x > 20)"))
    val res32 = LessThan(25) && or
    assert(res32 === c("(x < 10 && x > 0) || (x < 25 && x > 20)"))

    //((<10 && >0) || >20) && <9
    val res4 = or && LessThan(9)
    assert(res4 === c("x < 9 && x > 0"))
    val res42 = LessThan(9) && or
    assert(res42 === c("x < 9 && x > 0"))

    //((<10 && >0) || >20) && (>0 && < 10)
    val res5 =  or && c("x > 0 && x < 10")
    assert(res5 === c("x < 10 && x > 0"))
    val res52 =  c("x > 0 && x < 10") && or
    assert(res52 === c("x < 10 && x > 0"))

    //((<10 && >0) || >20) && ((>0 && < 10) || >25)
    val res6 = or && c("(x > 0 && x < 10) || x > 25")
    assert(res6 === c("(x < 10 && x > 0) || x > 25"))
  }

  test("Or SimpleConstraints") {
    val res1 = LessThan(10) || GreaterThan(20)
    assert(res1 === Or(List(And(LessThan(10)), And(GreaterThan(20)))))

    val res2 = LessThan(10) || LessThan(5)
    assert(res2 === LessThan(10))
  }

  test("Or And constraints") {
    val res1 = c("x > 0 && x < 10") || GreaterThan(20)
    assert(res1 === Or(List(And(GreaterThan(0), LessThan(10)), And(GreaterThan(20)))))
    val res12 = GreaterThan(20) || c("x > 0 && x < 10")
    assert(res12 === Or(List(And(GreaterThan(0), LessThan(10)), And(GreaterThan(20)))))

    val res2 = c("x > 0 && x < 10") || GreaterThan(0)
    assert(res2 === Or(List(And(GreaterThan(0), LessThan(10)), And(GreaterThan(0)))))
    val res22 = GreaterThan(0) || c("x > 0 && x < 10")
    assert(res22 === Or(List(And(GreaterThan(0), LessThan(10)), And(GreaterThan(0)))))

    val res3 = c("x > 0 && x < 10") || c("x > 0 && x < 10")
    assert(res3 === c("x > 0 && x < 10"))

    assert(!c("x > 1 && x < 11").definitelySubsetOf(LessThan(10)))
    val res4 = c("x > 0 && x < 10") || c("x > 1 && x < 11")
    assert(res4 === Or(List(And(GreaterThan(0), LessThan(10)), And(GreaterThan(1), LessThan(11)))))

  }

  test("Or Or constraints") {
    val or = Or(List(
      And(LessThan(10), GreaterThan(0)),
      And(GreaterThan(20))
    ))

    //((<10 && >0) || >20) || >25
    val res1 = or || GreaterThan(25)
    assert(res1 === or)
    val res12 = GreaterThan(25) || or
    assert(res12 === or)

    //((<10 && >0) || >20) || >15
    val res2 = or || GreaterThan(15)
    assert(res2 === c("(x < 10 && x > 0) || x > 15"))
    val res22 = GreaterThan(15) || or
    assert(res22 === c("(x < 10 && x > 0) || x > 15"))

    //((<10 && >0) || >20) || >15
    val res3 = or || LessThan(15)
    assert(res3 === c("x < 15 || x > 20"))
    val res32 = LessThan(15)|| or
    assert(res32 === c("x < 15 || x > 20"))

    //((<10 && >0) || >20) || (>21 && <30)
    val res4 = or || c("x > 21 && x < 30")
    assert(res4 === or)
    val res42 = c("x > 21 && x < 30") || or
    assert(res42 === or, res42.prettyPrint())

    //((<10 && >0) || >20) || ((>21 && <30) || < -10)
    val res5 = or || c("(x > 21 && x < 30) || x < -10")
    assert(res5 === Or(or.constraints :+ And(LessThan(-10))))
    val res52 = c("(x > 21 && x < 30) || x < -10") || or
    assert(res52 === Or(And(LessThan(-10)) :: or.constraints), res52.prettyPrint())
  }

  test("AND Corner cases") {
    val res1 = c("x > 0 && x < 10") && ImpossibleConstraint
    assert(res1 === ImpossibleConstraint)

    val res2 = c("x > 0 && x < 10") && NoConstraints
    assert(res2 === c("x > 0 && x < 10"))

    val res3 = c("x > 0 && x < 10") || ImpossibleConstraint
    assert(res3 === c("x > 0 && x < 10"))

    val res4 = c("x > 0 && x < 10") || NoConstraints
    assert(res4 === NoConstraints)

    val res5 = c("x > -1") && c("x <= 0")
    assert(res5 === And(GreaterThan(-1), LessThanOrEqual(0)), res5.prettyPrint())

    val res6 = c("x > -1") && c("x < 0")
    assert(res6 === And(GreaterThan(-1), LessThan(0)))
  }

  test("OR Corner cases") {
    val res1 = c("x < 0 || x > 10") && ImpossibleConstraint
    assert(res1 === ImpossibleConstraint)

    val res2 = c("x > 0 || x < 10") && NoConstraints
    assert(res2 === c("x > 0 || x < 10"))

    val res3 = c("x > 0 || x < 10") || ImpossibleConstraint
    assert(res3 === c("x > 0 || x < 10"))

    val res4 = c("x > 0 || x < 10") || NoConstraints
    assert(res4 === NoConstraints)
  }

  test("Negate constraints") {
    val res1 = !c("x < 0 || x > 10")
    assert(res1 === c("x >= 0 && x <= 10"))

    val res2 = !c("x < 0 && x > 10")
    assert(res2 === NoConstraints)

    val res3 = !c("x == 5")
    assert(res3 === c("x < 5 || x > 5"))
  }

  test("Map") {
    def inc(expr: ExpressionConstraint) =
      expr.expression + Polynomial.fromConstant(1)

    val res1 = c("x > 0 && x < 10").map(inc)
    assert(res1 === c("x > 1 && x < 11"))

    val res2 = c("x < 0 || x > 10").map(inc)
    assert(res2 === c("x < 1 || x > 11"))

    val res3 = c("(x > 0 && x < 10) || x > 20").map(inc)
    assert(res3 === c("(x > 1 && x < 11) || x > 21"))
  }

  test("FlatMap") {
    val res1 = for {
      ec1 <- c("x > 0 && x < 10")
      ec2 <- c("x > 1")
    } yield ec1 && ec2

    assert(res1 === c("x > 1 && x < 10"), res1.prettyPrint())

    val res2 = for {
      ec1 <- c("x < 0 || x > 10")
      ec2 <- c("x > 1")
    } yield ec1 && ec2

    assert(res2 === c("x > 10"), res2.prettyPrint())

    val res3 = for {
      ec1 <- c("x < 0 || x > 10")
      ec2 <- c("x > 1")
    } yield ec1 || ec2

    assert(res3 === c("x < 0 || x > 1"), res3.prettyPrint())

    val res4 = for {
      ec1 <- c("(x > 0 && x < 10) || x > 20")
      ec2 <- c("x > 5 && x < 25")
    } yield ec1 && ec2

    assert(res4 === c("(x > 5 && x < 10) || (x > 20 && x < 25)"), res4.prettyPrint())

    val res5 = for {
      ec1 <- c("x == y")
      ec2 <- c("(x >= 0 && x < 10) && x == z")
    } yield ec2

//    assert(res5 === c("(x > 5 && x < 10) || (x > 20 && x < 25)"), res4.prettyPrint())
  }

  test("Adding expression constraints") {
    val res1 = ec("x < 10") + ec("x < 11")
    assert(res1 === ec("x < 21"))

    val res2 = ec("x < 10") + ec("x > 10")
    assert(res2 === NoConstraints)

    val res3 = ec("x <= 10") + ec("x < 2")
    assert(res3 === ec("x < 12"))

    val res4 = ec("x > 10") + ec("x > 11")
    assert(res4 === ec("x > 21"))

    val res5 = ec("x > 10") + ec("x < 10")
    assert(res5 === NoConstraints)

    val res6 = ec("x >= 10") + ec("x > 2")
    assert(res6 === ec("x > 12"))
  }

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
