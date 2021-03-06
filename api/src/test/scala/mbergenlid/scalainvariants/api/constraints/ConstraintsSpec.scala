package mbergenlid.scalainvariants.api.constraints

import mbergenlid.scalainvariants.api.SymbolChain
import mbergenlid.scalainvariants.api.expressions.ConstantValue
import org.scalatest.FunSuite
import mbergenlid.scalainvariants.api.util.{TestUniverse, TestExpressionParser}

class ConstraintsSpec extends FunSuite
   with TestExpressionParser with TestUniverse {

  import Constraint._
  import scala.language.implicitConversions

  test("Impossible constraints") {
    assert((LessThan(0) && GreaterThanOrEqual(0)) === ImpossibleConstraint)
    assert((LessThanOrEqual(0) && GreaterThan(0)) === ImpossibleConstraint)
    assert((GreaterThan(0) && LessThanOrEqual(0)) === ImpossibleConstraint)
    assert((GreaterThanOrEqual(0) && LessThan(0)) === ImpossibleConstraint)
  }

  test("ExpressionConstraint.&&") {
    val res1 = LessThanOrEqual(1) && GreaterThanOrEqual(1)
    assert(res1 === Equal(1))

    val res2 = LessThanOrEqual(0) && GreaterThan(0)
    assert(res2 === ImpossibleConstraint)
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
    assert(res6 === ImpossibleConstraint)

    val res7 = c("x >= 10") && c("x == 9")
    assert(res7 === ImpossibleConstraint)
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

    //(A || B) && (C || D) ==> AC || AD || BC || BD
    //(>= 10 || == 9) && (>= 10 || == 9) ==> (>= 10 || == 9)
    val res7 = c("x >= 10 || x == 9") && c("x >= 10 || x == 9")
    assert(res7 === c("x >= 10 || x == 9"))

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

    val res7 = c("x < 1 && x >= -1") && c("x < 0")
    assert(res7 === c("x < 0 && x >= -1"))

    val res8 = c("x < 1 && x >= -1") && c("x >= 0")
    assert(res8 === c("x >= 0 && x < 1"))

    assert(c("x > 0 && x <= 1") == And(List(GreaterThan(0), LessThanOrEqual(1))))
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

  private def assertFactory(
    actual: Option[Expression => ExpressionConstraint],
    expected: Option[Expression => ExpressionConstraint]) {
      if(actual.isDefined && expected.isDefined) {
        assert(actual.get.apply(10) === expected.get.apply(10))
      } else {
        assert(!actual.isDefined && !expected.isDefined)
      }
  }

  test("<| operator") {
    assertFactory(ec("x > a") <| ec("x < 12"), Some(LessThan))
    assertFactory(ec("x < a") <| ec("x < 12"), None)
    assertFactory(ec("x <= a") <| ec("x < 12"), None)

    assertFactory(GreaterThan(stringToExpression("x") + 5) <| ec("x < 12"), Some(LessThan))

    //a <= x1 < x2 <= 12 ==> a < 12
    assertFactory(ec("x >= a") <| ec("x <= 12"), Some(LessThan))
    //a <= x1 < x2 == 12 ==> a < 12
    assertFactory(ec("x >= a") <| ec("x == 12"), Some(LessThan))

    // a > x1 < x2 > 12 ==> NoConstraint
    assertFactory(ec("x < a") <| ec("x > 12"), None)
    assertFactory(ec("x <= a") <| ec("x > 12"), None)

    //a == x1 < x2 < 12 ==> a < 12
    assertFactory(ec("x == a") <| ec("x < 12"), Some(LessThan))
    //a == x1 < x2 == 12 ==> a < 12
    assertFactory(ec("x == a") <| ec("x == 12"), Some(LessThan))
  }

  test("<=| operator") {
    //a < x1 <= x2 < 12 ==> a < 12
    assertFactory(ec("x > a") <=| ec("x < 12"), Some(LessThan))
    assertFactory(ec("x < a") <=| ec("x < 12"), None)
    assertFactory(ec("x <= a") <=| ec("x < 12"), None)

    //a <= x1 <= x2 <= 12 ==> a <= 12
    assertFactory(ec("x >= a") <=| ec("x <= 12"), Some(LessThanOrEqual))
    //a <= x1 <= x2 == 12 ==> a < 12
    assertFactory(ec("x >= a") <=| ec("x == 12"), Some(LessThanOrEqual))

    // a > x1 <= x2 > 12 ==> NoConstraint
    assertFactory(ec("x < a") <=| ec("x > 12"), None)
    assertFactory(ec("x <= a") <=| ec("x > 12"), None)

    //a == x1 <= x2 < 12 ==> a < 12
    assertFactory(ec("x == a") <=| ec("x < 12"), Some(LessThan))
    //a == x1 <= x2 == 12 ==> a < 12
    assertFactory(ec("x == a") <=| ec("x == 12"), Some(LessThanOrEqual))
  }

  test(">| operator") {
    assertFactory(ec("x < a") >| ec("x > 12"), Some(GreaterThan))
    assertFactory(ec("x > a") >| ec("x > 12"), None)
    assertFactory(ec("x >= a") >| ec("x > 12"), None)

    //a >= x1 > x2 >= 12 ==> a > 12
    assertFactory(ec("x <= a") >| ec("x >= 12"), Some(GreaterThan))
    //a >= x1 > x2 == 12 ==> a > 12
    assertFactory(ec("x <= a") >| ec("x == 12"), Some(GreaterThan))

    // a > x1 < x2 > 12 ==> NoConstraint
    assertFactory(ec("x > a") >| ec("x < 12"), None)
    assertFactory(ec("x >= a") >| ec("x < 12"), None)

    //a == x1 > x2 > 12 ==> a < 12
    assertFactory(ec("x == a") >| ec("x > 12"), Some(GreaterThan))
    //a == x1 > x2 == 12 ==> a > 12
    assertFactory(ec("x == a") >| ec("x == 12"), Some(GreaterThan))
  }

  test(">=| operator") {
    //a > x1 >= x2 > 12 ==> a > 12
    assertFactory(ec("x < a") >=| ec("x > 12"), Some(GreaterThan))
    assertFactory(ec("x > a") >=| ec("x > 12"), None)
    assertFactory(ec("x >= a") >=| ec("x > 12"), None)

    //a >= x1 >= x2 >= 12 ==> a >= 12
    assertFactory(ec("x <= a") >=| ec("x >= 12"), Some(GreaterThanOrEqual))
    //a >= x1 >= x2 == 12 ==> a > 12
    assertFactory(ec("x <= a") >=| ec("x == 12"), Some(GreaterThanOrEqual))

    // a < x1 >= x2 < 12 ==> NoConstraint
    assertFactory(ec("x > a") >=| ec("x < 12"), None)
    assertFactory(ec("x >= a") >=| ec("x < 12"), None)

    //a == x1 >= x2 > 12 ==> a > 12
    assertFactory(ec("x == a") >=| ec("x > 12"), Some(GreaterThan))
    //a == x1 >= x2 == 12 ==> a > 12
    assertFactory(ec("x == a") >=| ec("x == 12"), Some(GreaterThanOrEqual))
  }

  def t(v: Int, s: String, mult: Int) =
    Term(ConstantValue(v), Map(stringToSymbol(s) -> mult))

  def s(s: String) = stringToSymbol(s)

  test("ExpressionConstraint substitute") {
    val ec = Equal(Polynomial(t(1, "x", -1)))
    val res = ec.substitute(stringToSymbol("x"), Equal(2))
    assert(res === Some(Equal(Polynomial(Term(ConstantValue(0), Map())))))

    assert(GreaterThan(0).substitute(s("x"), Equal(2)) === Some(GreaterThan(0)))
  }
}
