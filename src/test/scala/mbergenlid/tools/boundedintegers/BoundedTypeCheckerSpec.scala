package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror

class BoundedTypeCheckerSpec extends FunSuite {
  
  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val cut = new BoundedTypeChecker(tb.u)

  val testProgram = 
    """
    |import mbergenlid.tools.boundedintegers.BoundedTypeChecker.Bounded
    |
    |def testMethod(@Bounded(min=0, max=10)a: Int, b: String) = 1
    |
    |testMethod(11, "hello")
    """.stripMargin

  def compile(program: String = testProgram) =
    tb.typeCheck(tb.parse(program)).asInstanceOf[cut.global.Tree]

  test("Extract function params") {
    import cut.global._
    val tree = compile()
    val Block((_, Apply(method, args))) = tree
    
    val argList = cut.extractMethodParams(
      method.asInstanceOf[cut.global.Tree], args.asInstanceOf[List[cut.global.Tree]])

    assert(argList.size === 2)
  }

  test("Failure with constant argument") {
    import BoundedTypeChecker._
    val tree = compile()
    
    val result = cut.checkBoundedTypes(tree)
    assert(result.size === 1)
  }
}
