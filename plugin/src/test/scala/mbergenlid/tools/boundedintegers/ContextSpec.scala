package mbergenlid.tools.boundedintegers


import org.scalatest.FunSuite

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
      LessThan(Polynom.fromConstant(4))
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
      "y" -> BoundedInteger(GreaterThan(Polynom.fromConstant(4)))
    ))

    val xBounds = Context.getBoundedInteger("x", c)
    assert(xBounds === originalXBound)
  }

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(LessThan(Polynom.fromSymbol("y"))),
      "y" -> BoundedInteger(LessThan(Polynom.fromConstant(4)))
    )))(LessThan(Polynom.fromConstant(4)))()

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(LessThan(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(LessThan(Polynom.fromConstant(4)))
    )))(LessThan(Polynom.fromConstant(8)))(LessThan(Polynom.fromConstant(4)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(LessThan(Polynom.fromSymbol("y"))),
      "y" -> BoundedInteger(LessThan(Polynom(Set(t(1, "z"), t(4))))),
      "z" -> BoundedInteger(LessThan(Polynom.fromConstant(1)))
    )))(LessThan(Polynom.fromConstant(5)))(LessThan(Polynom.fromConstant(4)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(LessThan(Polynom.fromConstant(4)))
    )))(LessThan(Polynom.fromConstant(8)))(LessThan(Polynom.fromConstant(4)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(GreaterThan(Polynom.fromConstant(4)))
    )))(GreaterThan(Polynom.fromConstant(8)))(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(Equal(Polynom.fromConstant(4)))
    )))(
      Equal(Polynom.fromConstant(8)), LessThan(Polynom.fromConstant(9))
    )(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(GreaterThan(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(GreaterThan(Polynom.fromConstant(4)))
    )))(GreaterThan(Polynom.fromConstant(8)))(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(GreaterThanOrEqual(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(GreaterThan(Polynom.fromConstant(4)))
    )))(GreaterThan(Polynom.fromConstant(8)))(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(And(LessThan(Polynom.fromConstant(10)), GreaterThan(Polynom.fromConstant(0))))
    ))) (
      LessThan(Polynom.fromConstant(14)), GreaterThan(Polynom.fromConstant(4))
    )()

  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4))))),
      "y" -> BoundedInteger(Or(LessThan(Polynom.fromConstant(0)), GreaterThan(Polynom.fromConstant(10))))
    ))) ()(LessThan(Polynom.fromConstant(4)))

  /**
   * x = y - 5
   * y >= 0 && <= 5
   *
   * x >= -5 && x <= 0
   */
  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(-5))))),
      "y" -> BoundedInteger(And(GreaterThanOrEqual(Polynom.fromConstant(0)), LessThanOrEqual(Polynom.fromConstant(5))))
    )))(GreaterThanOrEqual(Polynom.fromConstant(-5)))(GreaterThan(Polynom.fromConstant(-5)))

  /**
   * x > y
   * y >= 0 
   *
   * x > y && x > 0
   */
  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(GreaterThan(Polynom(Set(t(1, "y"))))),
      "y" -> BoundedInteger(GreaterThanOrEqual(Polynom.fromConstant(0)))
    )))(GreaterThan(Polynom.fromConstant(0)))()

  /**
   * x < y
   * y <= 0 
   *
   * x < y && x < 0
   */
  contextTest(
    new Context(Map(
      "x" -> BoundedInteger(LessThan(Polynom(Set(t(1, "y"))))),
      "y" -> BoundedInteger(LessThanOrEqual(Polynom.fromConstant(0)))
    )))(LessThan(Polynom.fromConstant(0)))()

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
      "y" -> BoundedInteger(And(GreaterThanOrEqual(Polynom.fromConstant(0)), LessThanOrEqual(Polynom.fromConstant(5)))),
      "z" -> BoundedInteger(And(GreaterThanOrEqual(Polynom.fromConstant(1)), LessThanOrEqual(Polynom.fromConstant(6))))
    )))(GreaterThanOrEqual(Polynom.fromConstant(1)), LessThan(Polynom.fromConstant(12)))(LessThanOrEqual(Polynom.fromConstant(10)))
  
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
      "y" -> BoundedInteger(GreaterThanOrEqual(Polynom.fromConstant(0))),
      "z" -> BoundedInteger(GreaterThan(Polynom.fromConstant(1)))
    )))(GreaterThanOrEqual(Polynom.fromSymbol("z")), GreaterThan(Polynom.fromConstant(1)))()

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
