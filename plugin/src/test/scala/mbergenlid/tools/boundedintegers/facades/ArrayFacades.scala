package mbergenlid.tools.boundedintegers.facades

import mbergenlid.tools.boundedintegers.PluginTestRunner

class ArrayFacades extends PluginTestRunner {

  test("Array access method") {
    compile(
      """
        |val a = Array(1,2,3,4,5)
        |
        |a(4)
        |
        |if(a.length > 5) a(4)
        |
        |true
      """.stripMargin)(List(4))
  }

  test("Array update method") {
    compile(
      """
        |val a = Array(1,2,3,4,5)
        |
        |a(4) = 1
        |
        |//if(a.length > 5) a(4) = 1
        |
        |true
      """.stripMargin)(List(4))
  }

}
