package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.language.implicitConversions

class BoundedTypeTreesSpec extends FunSuite 
    with BoundedTypeTrees {

  type Symbol = String

  implicit def intToConstant(v: Int) = ConstantValue(v)
  implicit def stringToSymbol(s: String) = SymbolExpression(s)

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

  test("Complex expressions") {
    //x > 0 && x < 100
    val e1 = And(GreaterThan(-1), LessThan(100))
    //(x > -1 && x < 10 && x <= 99)
    val e2 = And(And(GreaterThan(0), LessThan(10)), LessThanOrEqual(99))

    assert(e2 obviouslySubsetOf e1)
  }
}
