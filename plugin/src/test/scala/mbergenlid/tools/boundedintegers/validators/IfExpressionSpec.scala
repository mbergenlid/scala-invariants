package mbergenlid.tools.boundedintegers.validators


import mbergenlid.tools.boundedintegers._

class IfExpressionSpec extends PluginTestRunner {

  test("Result of if expression") {
    val bounds = expression(
      """
        |val x = anotherRandomInteger
        |
        |if(x > 0) x + 10
        |else -1
      """.stripMargin)

    assertThat(bounds.constraint).definiteSubsetOf(cut.GreaterThan(10) || cut.Equal(-1))
  }

  test("Result of negative if expression") {
    val bounds = expression(
      """
        |val x = anotherRandomInteger
        |val y = randomInteger
        |
        |if(x == 0) y - x
        |else -1
      """.stripMargin)

    import cut._
    assertThat(bounds.constraint).definiteSubsetOf(GreaterThanOrEqual(0) || Equal(-1))
  }


  test("Result of if expression with Block") {
    val bounds = expression(
      """
        |val x = anotherRandomInteger
        |
        |if(x > 0) {
        |  x + 10
        |} else -1
      """.stripMargin)

    assertThat(bounds.constraint).definiteSubsetOf(cut.GreaterThan(10) || cut.Equal(-1))
  }
}
