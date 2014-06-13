package mbergenlid.tools.boundedintegers

class OverrideSpec extends PluginTestRunner {

    test("Overridden field should inherit constraints") {

        compile(
            """
            |trait Super {
            |  @GreaterThan(0)
            |  def value: Int
            |}
            |
            |class Concrete extends Super {
            |  def value = 10
            |}
            |@GreaterThan(0)
            |val x = new Concrete().value
            |
            |true
        """.stripMargin)(Nil)
    }

}
