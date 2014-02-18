package mbergenlid.tools.boundedintegers.validators

import mbergenlid.tools.boundedintegers._

class ArithmeticExpressionSpec extends PluginTestRunner {

  ignore("Add constant to symbol") {
    compile("""
          |val x = intBetween0And5
          |
          |val y = x + 4
          |testMethod(y)
      """.stripMargin)(Nil)
  }
}
