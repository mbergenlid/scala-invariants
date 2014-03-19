package mbergenlid.tools.boundedintegers


class TransitiveConstraintSpec extends PluginTestRunner {

  test("Transitive constraints") {
    val program =
          """
          |val x = intBetween0And10
          |val y = anotherRandomInteger
          |val z = anotherRandomInteger
          |
          |if(y < x) {
          |  if(z > 0 && z < y) testMethod(z)
          |}
          """.stripMargin

    compile(program)(Nil)
  }

  test("Transitive in same boolean expression") {
    compile("""
          |val x = intBetween0And10
          |val y = anotherRandomInteger
          |val z = anotherRandomInteger
          |
          |if(y < x && z > 0 && z < y) testMethod(z)
          """.stripMargin)(Nil)
  }

  test("Deferred transitive constraint") {
    compile("""
          |val x = anotherRandomInteger
          |val y = anotherRandomInteger
          |val z = anotherRandomInteger
          |
          |if(x > 0 && x < y) {
          |  if(y < 10) testMethod(x)
          |}
          """.stripMargin)(Nil)
  }

  test("Cyclic transitive constraint") {
    compile("""
          |val x = anotherRandomInteger
          |val y = anotherRandomInteger
          |val z = anotherRandomInteger
          |
          |if(x > 0 && x < y) {
          |  if(y < x) testMethod(x)
          |}
          """.stripMargin)(List(7))
  }
}
