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

  test("Method constraint") {
    val bounds = expression(
      """
        |val x = randomInteger
        |
        |@Equal(x)
        |def myMethod() = x
        |
        |myMethod()
      """.stripMargin)

    import cut._
    assertThat(bounds.constraint).definiteSubsetOf(GreaterThanOrEqual(0))
  }

  test("Use input bound method") {
    compile(
      """
        |@Equal("n")
        |def myMethod(n: Int) = n
        |
        |@Equal(10)
        |val x = myMethod(10)
        |true
      """.stripMargin)(Nil)

    val b = expression(
      """
         |@Equal("n")
         |def myMethod(n: Int) = n
         |
         |myMethod(10)
       """.stripMargin)

    println(b.constraint.prettyPrint())
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

  test("Bound input to other input") {
    compile(
      """
        |def myMethod(from: Int, @GreaterThan("from") to: Int) = to - from
        |
        |myMethod(1,2)
        |myMethod(2,1)
        |true
      """.stripMargin)(List(5))
  }
}

