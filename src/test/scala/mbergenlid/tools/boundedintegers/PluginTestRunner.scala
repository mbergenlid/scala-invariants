package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror
import validators._


trait PluginTestRunner extends FunSuite
  with MyUniverse {
  
  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val global = tb.u
  val cut = new BoundedTypeChecker(tb.u) with MethodApplication
                                          with IfExpression
                                          with Assignment
                                          with ArithmeticExpression
                                         

  import cut.global._

  def typeCheck(program: String) =
    tb.typeCheck(tb.parse(program)).asInstanceOf[cut.global.Tree]

  def compile(program: String)(expectedErrorLines: List[Int] = Nil) {
    val withImports = 
      """|import mbergenlid.tools.boundedintegers.testclasspath.TestMethods._
         |import mbergenlid.tools.boundedintegers.Bounded
      """.stripMargin + program

    val errors = cut.checkBoundedTypes(typeCheck(withImports))
    val errorPositions = errors map (_.pos.line)
    val expectedErrorsAdjusted = expectedErrorLines map (_ + 2)
    if(errorPositions != expectedErrorsAdjusted) {
      fail(s"Expected: $expectedErrorsAdjusted\nGot: $errors")
    } 
  }
}
