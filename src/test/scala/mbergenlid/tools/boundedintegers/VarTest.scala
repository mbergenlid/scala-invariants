package mbergenlid.tools.boundedintegers


class VarTest extends PluginTestRunner {

  test("Simple var") {
    compile("""
      |var x = 5
      |testMethod(x)
      """.stripMargin)(List(3))
  }

  test("Simple var boundary") {
    compile("""
      |@Bounded(min=0, max=10)
      |var x = 5
      |
      |val y = anotherRandomInteger
      |if(y > 0 && y < x) testMethod(y)
      """.stripMargin)(Nil)
  }

  test("Re-assigning a bounded variable") {
    compile("""
      |@Bounded(min=0, max=10)
      |var x = 5
      |x = 100
      |x
      """.stripMargin)(List(4))
  }

  test("Modifying a bounded variable") {
    compile("""
      |@Bounded(min=0, max=10)
      |var x = 5
      |x = x + 2
      |x
      """.stripMargin)(List(4))
  }

  test("Modify var after check") {
    compile("""
      |var x = 5
      |def boundToVar(@Bounded(min=0, max=x)a: Int) = a == 1
      |
      |val y = anotherRandomInteger
      |if(y > 0 && y < x) {
      |  x -= 2
      |  boundToVar(y)
      |}
      """.stripMargin)(List(8))
  }

}