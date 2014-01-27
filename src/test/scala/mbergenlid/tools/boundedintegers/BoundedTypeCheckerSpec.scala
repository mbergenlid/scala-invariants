package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._

class BoundedTypeCheckerSpec extends FunSuite {
  
  val testProgram = 
    """
    |def testMethod(a: Int, b: String) = 1
    |
    |testMethod(3, "hello")
    """.stripMargin

  test("Extract function params") {
    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
    val tree = tb.typeCheck(tb.parse(testProgram))

    val Block((_, Apply(method, args))) = tree
    val cut = new BoundedTypeChecker(tb.u)
    
    val argList = cut.extractMethodParams(method.asInstanceOf[cut.global.Tree], args.asInstanceOf[List[cut.global.Tree]])

    assert(argList.size === 2)
  }
}
