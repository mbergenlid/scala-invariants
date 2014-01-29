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
    |testMethod(4, "In range")
    |
    |testMethod(11, "Over limit")
    |testMethod(-1, "Below limit")
    """.stripMargin

  def typeCheck(program: String = testProgram) =
    tb.typeCheck(tb.parse(program)).asInstanceOf[cut.global.Tree]

  def compile(program: String = testProgram): List[BoundedTypeChecker.BoundedTypeError] =
    cut.checkBoundedTypes(typeCheck(program))

  test("Extract function params") {
    import cut.global._
    val tree = typeCheck()
    val Block((_, Apply(method, args))) = tree

    val argList = cut.extractMethodParams(
      method.asInstanceOf[cut.global.Tree], args.asInstanceOf[List[cut.global.Tree]])

    assert(argList.size === 2)
  }

  test("Failure with constant argument") {
    val result = compile()
    assert(result.size === 2)
  }

  test("Should fail if called with variable that is not itself within range") {

    val program =
          """
          |import mbergenlid.tools.boundedintegers.BoundedTypeChecker.Bounded
          |
          |def testMethod(@Bounded(min=0, max=10)a: Int, b: String) = 1
          |
          |val x = 11
          |testMethod(x, "Invalid variable")
          |
          """.stripMargin

    val result = compile(program)
    assert(result.size === 1)
  }
}
