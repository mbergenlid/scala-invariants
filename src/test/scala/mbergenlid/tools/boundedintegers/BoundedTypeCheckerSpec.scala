package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror

class BoundedTypeCheckerSpec extends FunSuite
  with MyUniverse {
  
  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val global = tb.u
  val cut = new BoundedTypeChecker(tb.u) with MethodApplication
                                          with IfExpression 

  val testProgram = 
    """
    |import mbergenlid.tools.boundedintegers.Bounded
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

  def compile(program: String = testProgram): List[cut.BoundedTypeError] =
    cut.checkBoundedTypes(typeCheck(program))


  test("Failure with constant argument") {
    val result = compile()
    assert(result.size === 2)
  }

  test("Should fail if called with variable that is not itself within range") {
    val program =
          """
          |import mbergenlid.tools.boundedintegers.Bounded
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

  test("Should not fail if variable is annotated") {
    val program =
          """
          |import mbergenlid.tools.boundedintegers.Bounded
          |
          |def testMethod(@Bounded(min=0, max=10)a: Int, b: String) = 1
          |
          |@Bounded(10, 10) val x = 10
          |testMethod(x, "Invalid variable")
          |
          """.stripMargin

    val result = compile(program)
    assert(result.size === 0)
  }

  test("Should fail if called with arbitrary Int expression") {
    val program =
          """
          |import mbergenlid.tools.boundedintegers.Bounded
          |
          |def testMethod(@Bounded(min=0, max=10)a: Int, b: String) = 1
          |
          |def randomInteger = 1
          |testMethod(randomInteger, "Invalid variable")
          |
          """.stripMargin

    val result = compile(program)
    assert(result.size === 1)
  }

}
