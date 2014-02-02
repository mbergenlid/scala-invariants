package mbergenlid.tools.boundedintegers


import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror

class BooleanExpressionEvaluatorSpec extends FunSuite
  with MyUniverse {
  
  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val global = tb.u
  val cut = new BoundedTypeChecker(tb.u) with BooleanExpressionEvaluator

  import cut.global._
  import cut.BoundedInteger._
  def typeCheck(program: String) =
    tb.typeCheck(tb.parse(program)).asInstanceOf[cut.global.Tree]

  def findSymbol(name: String, tree: Tree) = tree.find { t => 
    t.symbol != null && 
      t.symbol.name == stringToTermName(name)
  }.get.symbol

  test("Simple condition") {
    import cut.global._
    val program =
      """
      |val x = 1
      |x < 10
      """.stripMargin
      
      val Block((list, cond @ Apply(Select(xSymb, _), _))) = typeCheck(program)
      val context = cut.evaluate(cond)

      val Some(x) = context(xSymb.symbol)
      assert(x.min === Int.MinValue)
      assert(x.max === 9)
      assert(Int.MinValue <:< x)
      assert(9 <:< x)
  }

  test("Condition with conjunction") {
    val program =
      """
      |val x = 1
      |x < 10 && x > 0
      """.stripMargin
      
      val tree @ Block((_, cond)) = typeCheck(program)
      val context = cut.evaluate(cond)

      val Some(x) = context(findSymbol("x", cond))
      assert(x.min === 1)
      assert(x.max === 9)
      assert(1 <:< x)
      assert(9 <:< x)
  }

  test("Condition with disjunction") {
    val program =
      """
      |val x = 1
      |x > 10 || x < 0
      """.stripMargin
      
      val tree @ Block((_, cond)) = typeCheck(program)
      val context = cut.evaluate(cond)

      val Some(x) = context(findSymbol("x", cond))
      assert(x.min === Int.MinValue)
      assert(x.max === Int.MaxValue)
      assert(-1 <:< x)
      assert(11 <:< x)
      assert(!(4 <:< x))
  }
}
