package mbergenlid.tools.boundedintegers


import org.scalatest.FunSuite
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror

class ContextSpec extends FunSuite 
    with TypeContext with BoundedTypeTrees with Expressions {

  type BoundedSymbol = String
  def createBound(symbol: BoundedSymbol) =
    BoundedInteger.noBounds

  def t(v: Int) = Term(ConstantValue(v), Map.empty)
  def t(v: Int, s: String*) = Term(ConstantValue(v), (Map.empty[BoundedSymbol, Int] /: s) { (map, term) =>
    val multiplicity = map.getOrElse(term, 0) + 1
    map + (term -> multiplicity)
  })

  test("Simple retrieval") {
    val expected = BoundedInteger(
      LessThan(ConstantValue(4))
    )
    val c = new Context(Map(
      "x" -> expected
    ))

    assert(Context.getBoundedInteger("x", c) === expected)
  }

  test("More complex transitive with Mixed") {
    val originalXBound = BoundedInteger(LessThan(Polynom(Set(t(1, "y"), t(4)))))
    val c = new Context(Map(
      "x" -> originalXBound,
      "y" -> BoundedInteger(GreaterThan(ConstantValue(4)))
    ))

    val xBounds = Context.getBoundedInteger("x", c)
    assert(xBounds === originalXBound)
  }

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(LessThan(SymbolExpression("y"))),
      "y" -> BoundedInteger(LessThan(ConstantValue(4)))
    )))(LessThan(ConstantValue(4)))()

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(LessThan(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(LessThan(ConstantValue(4)))
    )))(LessThan(ConstantValue(8)))(LessThan(ConstantValue(4)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(LessThan(SymbolExpression("y"))),
      "y" -> BoundedInteger(LessThan(Polynom(Set(t(1, "z"), t(4))))),
      "z" -> BoundedInteger(LessThan(ConstantValue(1)))
    )))(LessThan(ConstantValue(5)))(LessThan(ConstantValue(4)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(LessThan(ConstantValue(4)))
    )))(LessThan(ConstantValue(8)))(LessThan(ConstantValue(4)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(GreaterThan(ConstantValue(4)))
    )))(GreaterThan(ConstantValue(8)))(GreaterThan(ConstantValue(9)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(Equal(ConstantValue(4)))
    )))(
      Equal(ConstantValue(8)), LessThan(ConstantValue(9))
    )(GreaterThan(ConstantValue(9)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(GreaterThan(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(GreaterThan(ConstantValue(4)))
    )))(GreaterThan(ConstantValue(8)))(GreaterThan(ConstantValue(9)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(GreaterThanOrEqual(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(GreaterThan(ConstantValue(4)))
    )))(GreaterThan(ConstantValue(8)))(GreaterThan(ConstantValue(9)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(And(LessThan(ConstantValue(10)), GreaterThan(ConstantValue(0))))
    ))) (
      LessThan(ConstantValue(14)), GreaterThan(ConstantValue(4))
    )()

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(Or(LessThan(ConstantValue(0)), GreaterThan(ConstantValue(10))))
    ))) ()(LessThan(ConstantValue(4)))

  /**
   * x = y - 5
   * y >= 0 && <= 5
   *
   * x >= -5 && x <= 0
   */
  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(-5))))),
      "y" -> BoundedInteger(And(GreaterThanOrEqual(ConstantValue(0)), LessThanOrEqual(ConstantValue(5))))
    )))(GreaterThanOrEqual(ConstantValue(-5)))(GreaterThan(ConstantValue(-5)))

  /**
   * x > y
   * y >= 0 
   *
   * x > y && x > 0
   */
  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(GreaterThan(Polynom(Set(t(1, "y"))))),
      "y" -> BoundedInteger(GreaterThanOrEqual(ConstantValue(0)))
    )))(GreaterThan(ConstantValue(0)))()

  /**
   * x < y
   * y <= 0 
   *
   * x < y && x < 0
   */
  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(LessThan(Polynom(Set(t(1, "y"))))),
      "y" -> BoundedInteger(LessThanOrEqual(ConstantValue(0)))
    )))(LessThan(ConstantValue(0)))()

  /**
   * _ = y + z
   * y >= 0 && y <= 5
   * z >= 1 && z <= 6
   *
   * _ >= 1
   */
  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(1, "z"))))),
      "y" -> BoundedInteger(And(GreaterThanOrEqual(ConstantValue(0)), LessThanOrEqual(ConstantValue(5)))),
      "z" -> BoundedInteger(And(GreaterThanOrEqual(ConstantValue(1)), LessThanOrEqual(ConstantValue(6)))) 
    )))(GreaterThanOrEqual(ConstantValue(1)), LessThan(ConstantValue(12)))(LessThanOrEqual(ConstantValue(10)))
  
  /**
   * _ = y + z
   * y >= 0 && y <= 5
   * z >= 1 && z <= 6
   *
   * _ >= 1
   */
  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(1, "z"))))),
      "y" -> BoundedInteger(GreaterThanOrEqual(ConstantValue(0))),
      "z" -> BoundedInteger(GreaterThan(ConstantValue(1))) 
    )))(GreaterThanOrEqual(SymbolExpression("z")), GreaterThan(ConstantValue(1)))()

  def contextTest(c: Context, debug: Boolean = false)(positiveAsserts: Constraint*)(negativeAsserts: Constraint*) {
    test(c.toString) {
      val xBounds = Context.getBoundedInteger("x", c).constraint
      if(debug) {
        println(xBounds.prettyPrint("x"))
      }
      assert(positiveAsserts.forall ( xBounds obviouslySubsetOf _ ))
      negativeAsserts.foreach {x => 
        assert(!(xBounds obviouslySubsetOf x),
          s"Expected ${xBounds.prettyPrint("x")} to not be a subset of ${x.prettyPrint("x")}")
      }
    }
  }
}
