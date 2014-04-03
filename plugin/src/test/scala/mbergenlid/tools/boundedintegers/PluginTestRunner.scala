package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror
import validators._
import scala.language.implicitConversions
import scala.language.reflectiveCalls

trait PluginTestRunner extends FunSuite {

  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val global = tb.u
  val cut = new BoundedTypeChecker(tb.u) with MethodApplication
                                          with IfExpression
                                          with Assignment
                                          with ArithmeticExpression
                                          with MethodDefinition

  import cut._
                                         

  def typeCheck(program: String) =
    tb.typeCheck(tb.parse(program)).asInstanceOf[cut.global.Tree]

  def compile(program: String)(expectedErrorLines: List[Int] = Nil) {
    val withImports =
      """|import mbergenlid.tools.boundedintegers.testclasspath.TestMethods._
         |import mbergenlid.tools.boundedintegers.annotations._
         |println("Start of test")
      """.stripMargin + program

    val errors = cut.checkBoundedTypes(typeCheck(withImports))
    val errorPositions = errors map (_.pos.line)
    val expectedErrorsAdjusted = expectedErrorLines map (_ + 3)
    if(errorPositions != expectedErrorsAdjusted) {
      fail(s"Expected: $expectedErrorsAdjusted\nGot: $errors")
    } 
  }

  def expression(expression: String) = {
    val withImports =
      """|import mbergenlid.tools.boundedintegers.testclasspath.TestMethods._
        |import mbergenlid.tools.boundedintegers.annotations._
        |println("Start of test")
      """.stripMargin + expression

    cut.checkBounds(new cut.Context())(typeCheck(withImports))
  }

  implicit def int2Expression(v: Int) =
    Polynomial.fromConstant(v)

  trait ConstraintAssert[C1 <: cut.Constraint] {
    def definiteSubsetOf[C2 <: cut.Constraint](other: C2): Unit
  }
  def assertThat[C1 <: cut.Constraint](c: C1) = new ConstraintAssert[C1] {
    def definiteSubsetOf[C2 <: cut.Constraint](other: C2) {
      assert(c obviouslySubsetOf other,
        s"Expected ${c.prettyPrint("x")} to be a subset of ${other.prettyPrint("x")}")
    }
  }
}
