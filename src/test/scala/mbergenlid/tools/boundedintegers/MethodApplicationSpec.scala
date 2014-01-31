package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror

class MethodApplicationSpec extends FunSuite
  with MyUniverse {
  
  def tb = TestEnvironment.tb
  implicit val global = tb.u
  val cut = new BoundedTypeChecker(global) with MethodApplication

  def typeCheck(program: String) =
    tb.typeCheck(tb.parse(program)).asInstanceOf[cut.global.Tree]

  test("Extract function params") {
    import cut.global._
    val testProgram = 
      """
      |import mbergenlid.tools.boundedintegers.Bounded
      |
      |def testMethod(@Bounded(min=0, max=10)a: Int, b: String) = 1
      |testMethod(-1, "Below limit")
      """.stripMargin

    val tree = typeCheck(testProgram)
    val Block((_, Apply(method, args))) = tree

    val argList = cut.extractMethodParams(
      method.asInstanceOf[cut.global.Tree], args.asInstanceOf[List[cut.global.Tree]])

    assert(argList.size === 2)
  }
}