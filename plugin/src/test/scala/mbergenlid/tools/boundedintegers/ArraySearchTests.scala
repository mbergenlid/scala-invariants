package mbergenlid.tools.boundedintegers

class ArraySearchTests extends PluginTestRunner {

  test("Recursive linear search") {
    compile(
      """
        |def find(
        |  array: Array[Int],
        |  element: Int,
        |  @GreaterThanOrEqual(0) @LessThanOrEqual(array.length) start: Int): Int = {
        |
        |    if(start == array.length) -1
        |    else if(array(start) == element) start
        |    else find(array, element, start+1)
        |}
        |
        |true
      """.stripMargin)(Nil)
  }
}
