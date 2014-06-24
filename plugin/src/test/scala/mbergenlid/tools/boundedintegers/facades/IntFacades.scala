package mbergenlid.tools.boundedintegers.facades

import mbergenlid.tools.boundedintegers.PluginTestRunner

class IntFacades extends PluginTestRunner {

  test("Basic test") {
   val bounds = expression(
     """
       |val y = 5
       |
       |y + 1
     """.stripMargin)

    println(bounds.constraint.prettyPrint())
  }
}
