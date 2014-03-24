package mbergenlid.tools.boundedintegers

class TypeTest extends PluginTestRunner {

  test("Simple Int + Double") {
    compile(
      """
        |val x = intBetween0And5
        |
        |@LessThan(6)
        |val y = x + 0.5
        |1
      """.stripMargin)(Nil)
  }

  ignore("Should not be able to annotate Strings") {
    compile(
      """
        |@LessThan(5)
        |val x = "ASD"
      """.stripMargin)(List(3))
  }
}
