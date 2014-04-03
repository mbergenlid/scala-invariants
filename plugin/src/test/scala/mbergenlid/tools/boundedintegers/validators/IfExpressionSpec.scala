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

  test("Bound to expression") {
    compile(
      """
        |val x = anotherRandomInteger
        |
        |if(x > 0 && x + 1 < 12)
        |  testMethod(x)
        |
        |if(x > 0 && x + 2 < 14)
        |  testMethod(x)
      """.stripMargin)(List(8))
  }

  /**
   * x < 15 + 5
   * x < 20
   *
   * x < 15 - [0,5]
   *   < 15
   *
   */
  test("Bound to symbol expression") {
    compile(
      """
        |val x = anotherRandomInteger
        |val y = intBetween0And5
        |
        |if(x > 0 && x + intBetween0And5 < 15)
        |  testMethod(x)  //error
        |
        |if(x > 0 && x + intBetween5And10 < 15)
        |  testMethod(x)
        |
        |if(x < 11 && x + intBetween5And10 > 10)
        |  testMethod(x)
        |
        |if(x < 11 && x + intBetween0And5 > 2)
        |  testMethod(x)  //error
      """.stripMargin)(List(6, 15))
  }

  test("Bound to symbol expression with <=") {
    compile(
      """
        |val x = anotherRandomInteger
        |val y = intBetween0And5
        |
        |if(x > 0 && x + intBetween0And5 <= 15)
        |  testMethod(x)  //error
        |
        |if(x > 0 && x + intBetween5And10 <= 15)
        |  testMethod(x)
        |
        |if(x < 11 && x + intBetween5And10 >= 10)
        |  testMethod(x)
        |
        |if(x < 11 && x + intBetween0And5 >= 2)
        |  testMethod(x)  //error
      """.stripMargin)(List(6, 15))
  }

  test("Bound to symbol expression with ==") {
    compile(
      """
        |val x = anotherRandomInteger
        |val y = intBetween0And5
        |
        |if(x + intBetween0And5 == 15)
        |  testMethod(x)  //error
        |
        |if(x + intBetween0And5 == 10)
        |  testMethod(x)
      """.stripMargin)(List(6))
  }
}
