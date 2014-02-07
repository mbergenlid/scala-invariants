package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

class BoundedTypeTreesSpec extends FunSuite 
    with BoundedTypeTrees {

  type Symbol = String

  implicit def intToConstant(v: Int) = ConstantValue(v)
  implicit def stringToSymbol(s: String) = SymbolExpression(s)

  class ExprParser extends JavaTokenParsers {
      
    def value: Parser[Expression] =
      (ident ^^ SymbolExpression) | (wholeNumber ^^ {x: String => ConstantValue(x.toInt)} )
  }


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
  
  test("Simple constraints") {
    val c1 = GreaterThan(ConstantValue(0))
    val c2 = LessThanOrEqual(ConstantValue(10))
    val c3 = GreaterThanOrEqual(ConstantValue(1))

    assert(!(c1 obviouslySubsetOf c2), s"$c1 obviouslySubsetOf $c2")
    assert(c3 obviouslySubsetOf c1, s"$c3 obviouslySubsetOf $c1")
  }

  test("Complex AND expressions") {
    //x > 0 && x < 100
    val e1 = And(GreaterThan(0), LessThan(100))
    //(x > -1 && x < 10 && x < y && x <= 99)
    val e2 = And(And(And(GreaterThan(0), LessThan(10)), LessThan("y")), LessThanOrEqual(99))

    assert(e2 obviouslySubsetOf e1)
  }

  test("Complex OR expressions") {
    //x < 0 || x > 100
    val e1 = Or(LessThan(0), GreaterThan(100))
    //(x >= 101)
    val e2 = GreaterThanOrEqual(101)
    //(x <= -1 || x >= 101)
    val e3 = Or(LessThanOrEqual(-1), GreaterThanOrEqual(101))
    //(x <= -1 || x >= 101 || x < 6)
    val e4 = Or(Or(LessThanOrEqual(-1), GreaterThanOrEqual(101)), LessThan(6))

    assert(e2 obviouslySubsetOf e1)
    assert(e3 obviouslySubsetOf e1)
    assert(!(e4 obviouslySubsetOf e1))
  }

  test("Complex mixed expressions") {
    //(x > 0 && x < 100) || (x > -100 && x < -10)
    val e1 = Or(
      And(GreaterThan(0), LessThan(100)),
      And(GreaterThan(-100), LessThan(-10))
    );
    
    //(x > 1 && x <= 10)
    val e2 = And(GreaterThan(1), LessThanOrEqual(10))

    assert(e2 obviouslySubsetOf e1)
  }
}
