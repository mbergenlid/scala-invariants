package mbergenlid.tools.boundedintegers

class BoundToInputSpec extends PluginTestRunner {

  test("Bound to simple input") {
    compile(
      """
        |@Equal("n")
        |def myMethod1(n: Int) = n
        |
        |@Equal("n")
        |def myMethod2(n: Int) = n + 1
        |true
      """.stripMargin)(List(6))
  }

  test("Bound to double input") {
    compile(
      """
        |@Equal("n")
        |def myMethod1(n: Double) = n
        |
        |@Equal("n")
        |def myMethod2(n: Double) = n + 1
        |true
      """.stripMargin)(List(6))
  }
}

