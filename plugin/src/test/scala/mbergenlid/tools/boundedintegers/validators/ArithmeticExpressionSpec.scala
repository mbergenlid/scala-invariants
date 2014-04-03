package mbergenlid.tools.boundedintegers.validators

import mbergenlid.tools.boundedintegers._
import scala.language.reflectiveCalls

class ArithmeticExpressionSpec extends PluginTestRunner {

  test("Add constant to symbol") {
    compile("""
              |@GreaterThanOrEqual(0)
              |@LessThanOrEqual(5)
              |val x = intBetween0And5
              |
              |val y = x + 4
              |testMethod(y)
              |
              |testMethod(x + 4)
              |testMethod(x + 7)
            """.stripMargin)(List(10))
  }

  test("Subtract constant to symbol") {
    compile("""
              |@GreaterThanOrEqual(0)
              |@LessThanOrEqual(5)
              |val x = intBetween0And5
              |
              |@LessThanOrEqual(5)
              |@GreaterThanOrEqual(-5)
              |val y = x - 5
              |
              |@LessThanOrEqual(1000)
              |@GreaterThanOrEqual(-2)
              |val z = x - 5
              |1
            """.stripMargin)(List(12))
  }

  test("Multiply constant to symbol") {
    compile("""
              |val x = intBetween0And5
              |
              |@GreaterThanOrEqual(0)
              |@LessThanOrEqual(50)
              |val y = x * 10
              |
              |testMethod(x*10)
              |1
            """.stripMargin)(List(8))
  }

  test("Add symbol to symbol") {
    compile("""
              |val x = intBetween0And5
              |
              |@GreaterThanOrEqual(1)
              |@LessThanOrEqual(6)
              |val y = x + 1
              |testMethod(y + x)
              |
              |@GreaterThanOrEqual(1)
              |@LessThanOrEqual(11)
              |val z = x + y
              |1
            """.stripMargin)(List(7))
  }

  test("Subtract symbol from constant") {
    val bounds = expression(
      """
        |val x = intBetween0And5
        |10 - x
      """.stripMargin)

    assertThat(bounds.constraint) definiteSubsetOf cut.LessThan(11)
    assertThat(bounds.constraint).definiteSubsetOf(cut.GreaterThan(4))
    assert(!bounds.constraint.obviouslySubsetOf(cut.LessThanOrEqual(5)))
  }

  test("Multiply symbol to constant") {
    val bounds = expression(
      """
        |val x = intBetween0And5
        |2*x
      """.stripMargin)

    assertThat(bounds.constraint) definiteSubsetOf cut.GreaterThanOrEqual(0)
    assertThat(bounds.constraint).definiteSubsetOf(cut.LessThanOrEqual(10))
  }
}