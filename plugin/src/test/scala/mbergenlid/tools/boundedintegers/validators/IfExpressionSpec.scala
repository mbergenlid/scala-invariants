package mbergenlid.tools.boundedintegers.validators


import mbergenlid.tools.boundedintegers._
import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror

class IfExpressionSpec extends PluginTestRunner {
  
  test("Should succeed if inside appropriate if expression") {
    compile("""
          |val x: Int = anotherRandomInteger
          |if(x < 11) upperBoundMethod(x)
          |
          """.stripMargin)(Nil)
  }

  test("Test more complicated if expression") {
    compile("""
          |val x = anotherRandomInteger
          |if(x < 11 && -1 < x)
          |  testMethod(x)
          |else
          |  testMethod(x)
          """.stripMargin)(List(6))
  }

  test("Test successful else") {
    compile("""
          |val x = anotherRandomInteger
          |if(x > 10 || x < 0)
          |  testMethod(x)
          |else
          |  testMethod(x)
          """.stripMargin)(List(4))
  }

  test("Test bound to symbol") {
    compile("""
              |val maxValue = 10
              |def myMethod(@LessThanOrEqual(maxValue) @GreaterThanOrEqual(0L) a: Int, b: String) = 1
              |
              |val x = anotherRandomInteger
              |if(x > 0 && x < maxValue)
              |  myMethod(x, "Should compile")
              |
              |if(x > 0)
              |  myMethod(x, "Should not compile")
            """.stripMargin)(List(10))
  }

  test("Bound to symbol two-ways") {
    compile(
      """
        |val x = anotherRandomInteger
        |val y = 0
        |
        |if(x < 10 && y < x)
        |  testMethod(x)
      """.stripMargin)(Nil)
  }
}
