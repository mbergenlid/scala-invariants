package mbergenlid.tools.boundedintegers

class TypeTest extends PluginTestRunner {

  ignore("Simple Int + Double") {
    compile(
      """
        |val x = intBetween0And5
        |
        |@LessThan(6)
        |val y = x + 0.5
        |1
      """.stripMargin)(Nil)
  }
}
