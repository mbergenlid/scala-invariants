package mbergenlid.tools.boundedintegers.validators

import mbergenlid.tools.boundedintegers._

class ArithmeticExpressionSpec extends PluginTestRunner {

  test("Add constant to symbol") {
    compile("""
          |@Bounded(min=0, max=5)
          |val x = intBetween0And5
          |
          |val y = x + 4
          |testMethod(y)
          |
          |testMethod(x + 4)
          |testMethod(x + 7)
      """.stripMargin)(List(9))
  }

  test("Subtract constant to symbol") {
    compile("""
          |@Bounded(min=0, max=5)
          |val x = intBetween0And5
          |
          |@Bounded(min=(-4), max=5)
          |val y = x - 5
          |
          |@Bounded(min=(-2), max=1000)
          |val z = x - 5
          |1
      """.stripMargin)(List(9))
  }

  test("Multiply constant to symbol") {
    compile("""
          |@Bounded(min=0, max=5)
          |val x = intBetween0And5
          |
          |@Bounded(min=0, max=50)
          |val y = x * 10
          |
          |testMethod(x*10)
          |1
      """.stripMargin)(List(8))
  }

  test("Add symbol to symbol") {
    compile("""
          |@Bounded(min=0, max=5)
          |val x = intBetween0And5
          |
          |@Bounded(min=1, max=6)
          |val y = x + 1
          |testMethod(y + x)
          |
          |@Bounded(min=1, max=11)
          |val z = x + y
          |1
      """.stripMargin)(List(7))
  }
}
