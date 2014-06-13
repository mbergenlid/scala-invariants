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
            |val s: Super = new Concrete
            |
            |@GreaterThan(0)
            |val x = new Concrete().value //Overridden method value should inherit constraints from super
            |
            |@GreaterThan(0)
            |val y = s.value //Compile time type is now Super
            |true
        """.stripMargin)(Nil)
    }

    test("Overridden field should enforce constraints") {
        compile(
            """
            |trait Super {
            |  @GreaterThan(0)
            |  def value: Int
            |}
            |
            |class Concrete extends Super {
            |  def value = -1
            |}
            |
            |true
        """.stripMargin)(List(8))
    }

}
