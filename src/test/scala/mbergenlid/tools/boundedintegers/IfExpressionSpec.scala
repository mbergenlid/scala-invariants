package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror

class IfExpressionSpec extends FunSuite 
  with MyUniverse {
  
  def tb = TestEnvironment.tb
  implicit val global = tb.u
  val cut = new BoundedTypeChecker(global) with IfExpression

  def typeCheck(program: String) =
    tb.typeCheck(tb.parse(program)).asInstanceOf[cut.global.Tree]

  test("Simple less than condition") {
    import cut.global._
    val program =
      """ |val x = 15
          |if(x < 10) x else 10
      """.stripMargin

      val Block((List(x), If(cond, _, _))) = typeCheck(program)
      val context = cut.createContextFrom(cond)

      assert(context.size === 1)
      assert(context(x.symbol).get.min === Int.MinValue)
      assert(context(x.symbol).get.max === 10)
  }

  test("Simple greater than condition") {
    import cut.global._
    val program =
      """ |val x = 15
          |if(x > 10) x else 10
      """.stripMargin

      val Block((List(x), If(cond, _, _))) = typeCheck(program)
      val context = cut.createContextFrom(cond)

      assert(context.size === 1)
      assert(context(x.symbol).get.max === Int.MaxValue)
      assert(context(x.symbol).get.min === 10)
  }
}
