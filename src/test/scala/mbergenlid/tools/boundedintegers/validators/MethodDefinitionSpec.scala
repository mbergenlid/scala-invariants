package mbergenlid.tools.boundedintegers.validators

import mbergenlid.tools.boundedintegers.{MyUniverse, PluginTestRunner}

class MethodDefinitionSpec extends PluginTestRunner {
  self: MyUniverse =>

  test("Validate method return statement") {
    compile(
      """
        |@LessThanOrEqual(10)
        |def invalidMethod = 11
        |
        |@LessThanOrEqual(10)
        |def validMethod = 5
        |1
      """.stripMargin)(List(3))
  }

  test("Validate method return expression") {
    compile(
      """
        |val x = 11
        |
        |@LessThanOrEqual(10)
        |def inValidMethod = x
        |
        |@LessThanOrEqual(10)
        |def validMethod = x - 1
        |1
      """.stripMargin)(List(5))
  }
}
