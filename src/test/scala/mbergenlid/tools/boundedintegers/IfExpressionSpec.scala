package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror

class IfExpressionSpec extends FunSuite 
  with MyUniverse {
  
  def tb = TestEnvironment.tb
  implicit val global = tb.u
  val cut = new BoundedTypeChecker(global) with IfExpression
                                            with MethodApplication

  def typeCheck(program: String) =
    tb.typeCheck(tb.parse(program)).asInstanceOf[cut.global.Tree]

  def compile(program: String): List[cut.BoundedTypeError] =
    cut.checkBoundedTypes(typeCheck(program))

  test("Should succeed if inside appropriate if expression") {
    val program =
          """
          |import mbergenlid.tools.boundedintegers.Bounded
          |
          |def testMethod(@Bounded(min=Int.MinValue, max=9)a: Int, b: String) = 1
          |
          |def randomInteger = 1
          |val x = randomInteger
          |if(x < 10) testMethod(x, "Valid variable")
          |
          """.stripMargin

    val result = compile(program)
    assert(result.size === 0)
  }

  test("Test more complicated if expression") {
    val program =
          """
          |import mbergenlid.tools.boundedintegers.Bounded
          |
          |def testMethod(@Bounded(min=1, max=10)a: Int, b: String) = 1
          |
          |def randomInteger = 1
          |val x = randomInteger
          |if(x < 11 && x > 0)
          |  testMethod(x, "Valid variable")
          |else
          |  testMethod(x, "Invalid variable")
          """.stripMargin

    val result = compile(program)
    assert(result.size === 1)
  }

  test("Test successful else") {
    val program =
          """
          |import mbergenlid.tools.boundedintegers.Bounded
          |
          |def testMethod(@Bounded(min=0, max=10)a: Int, b: String) = 1
          |
          |def randomInteger = 1
          |val x = randomInteger
          |if(x > 10 || x < 0)
          |  println(x)
          |else
          |  testMethod(x, "Valid variable")
          """.stripMargin

    val result = compile(program)
    assert(result.size === 0)
  }
}
