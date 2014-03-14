package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror
import validators._


class BoundedTypeCheckerSpec extends PluginTestRunner
  with MyUniverse {
  
  test("Failure with constant argument") {
    compile("""
    |testMethod(4)
    |testMethod(11)
    |testMethod(-1)
    """.stripMargin)(List(3,4))
  }

  test("Should fail if called with variable that is not itself within range") {
    compile("""
          |val x = 11
          |testMethod(x)
          """.stripMargin)(List(3))
  }

  test("Should not fail if variable is annotated") {
    compile("""
              |@Equal(10) val x = 10
              |testMethod(x)
              |1
            """.stripMargin)(Nil)
  }

  test("Should fail if called with arbitrary Int expression") {
    compile("""
          |testMethod(anotherRandomInteger)
          |val x = anotherRandomInteger
          |testMethod(x)
          """.stripMargin)(List(2, 4))
  }

  test("Test return statement") {
    compile("""
              |@GreaterThanOrEqual(0)
              |@LessThanOrEqual(10)
              |val x = 11
              |1
            """.stripMargin)(List(4))
  }


  test("Validation in middle of boolean expression") { 
    compile("""
          |val x = randomInteger
          |
          |if(x < 10 && testMethod(x)) println("Should compile")
          |if(x > 0 && testMethod(x)) println("Should not compile")
          """.stripMargin)(List(5))
  }

}
