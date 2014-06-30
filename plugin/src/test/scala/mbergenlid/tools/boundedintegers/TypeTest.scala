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

  test("Decrement") {
    compile(
      """
        |@LessThanOrEqual(9)
        |def intMethod(@LessThan(10) x: Int) = x
        |
        |@LessThanOrEqual(9)
        |def method(@LessThan(10) x: Double) = x
        |
        |val x: Double = anotherRandomInteger
        |if(x < 10) {
        |  @LessThanOrEqual(10)
        |  val y = 1 + x
        |}
        |true
      """.stripMargin)(List(6, 11))
  }

  test("Increment") {
    compile(
      """
        |@GreaterThanOrEqual(10)
        |def intMethod(@GreaterThan(9) x: Int) = x
        |
        |@GreaterThanOrEqual(10)
        |def method(@GreaterThan(9) x: Double) = x
        |
        |val x: Double = anotherRandomInteger
        |if(x > 10) {
        |  @GreaterThanOrEqual(9)
        |  val y = 1 + x
        |}
        |true
      """.stripMargin)(List(6, 11))
  }

  ignore("Should not be able to annotate Strings") {
    compile(
      """
        |@LessThan(5)
        |val x = "ASD"
      """.stripMargin)(List(3))
  }
}
