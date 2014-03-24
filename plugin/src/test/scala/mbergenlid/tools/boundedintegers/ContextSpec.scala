package mbergenlid.tools.boundedintegers
import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

import scala.language.implicitConversions
class ContextSpec extends FunSuite 
    with TypeContext with BoundedTypeTrees with Expressions {

  type SymbolType = Symbol
  type TypeType = Type
  val TypeNothing = typeOf[Nothing]
  val IntSymbol = typeOf[Int].typeSymbol
  def createBound(symbol: SymbolType) =
    BoundedInteger.noBounds

  def expressionForType = {
    case TypeRef(_, IntSymbol, Nil) =>
      new ExpressionFactory[Int]
  }

  val x = 0
  val y = 0
  val z = 0
  implicit def sym(s: String): SymbolType =
    typeOf[this.type].member(newTermName(s))

  def t(v: Int) = Term(ConstantValue(v), Map.empty)
  def t(v: Int, s: String*) = Term(ConstantValue(v), (Map.empty[SymbolType, Int] /: s) { (map, term) =>
    val multiplicity = map.getOrElse(sym(term), 0) + 1
    map + (sym(term) -> multiplicity)
  })

  test("Simple retrieval") {
    val expected = BoundedInteger(
      LessThan(Polynom.fromConstant(4)), typeOf[Int]
    )
    val c = new Context(Map(
      sym("x") -> expected
    ))

    assert(Context.getBoundedInteger("x", typeOf[Int], c) === expected)
  }

  test("More complex transitive with Mixed") {
    val originalXBound = BoundedInteger(LessThan(Polynom(Set(t(1, "y"), t(4)))), typeOf[Int])
    val c = new Context(Map(
      sym("x") -> originalXBound,
      sym("y") -> BoundedInteger(GreaterThan(Polynom.fromConstant(4)), typeOf[Int])
    ))

    val xBounds = Context.getBoundedInteger("x", typeOf[Int], c)
    assert(xBounds === originalXBound)
  }

  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(LessThan(Polynom.fromSymbol[Int]("y")), typeOf[Int]),
      sym("y") -> BoundedInteger(LessThan(Polynom.fromConstant(4)), typeOf[Int])
    )))(LessThan(Polynom.fromConstant(4)))()

  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(LessThan(Polynom(Set(t(1, "y"), t(4)))), typeOf[Int]),
      sym("y") -> BoundedInteger(LessThan(Polynom.fromConstant(4)), typeOf[Int])
    )))(LessThan(Polynom.fromConstant(8)))(LessThan(Polynom.fromConstant(4)))

  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(LessThan(Polynom.fromSymbol[Int]("y")), typeOf[Int]),
      sym("y") -> BoundedInteger(LessThan(Polynom(Set(t(1, "z"), t(4)))), typeOf[Int]),
      sym("z") -> BoundedInteger(LessThan(Polynom.fromConstant(1)), typeOf[Int])
    )))(LessThan(Polynom.fromConstant(5)))(LessThan(Polynom.fromConstant(4)))

  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4)))), typeOf[Int]),
      sym("y") -> BoundedInteger(LessThan(Polynom.fromConstant(4)), typeOf[Int])
    )))(LessThan(Polynom.fromConstant(8)))(LessThan(Polynom.fromConstant(4)))

  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4)))),  typeOf[Int]),
      sym("y") -> BoundedInteger(GreaterThan(Polynom.fromConstant(4)), typeOf[Int])
    )))(GreaterThan(Polynom.fromConstant(8)))(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4)))), typeOf[Int]),
      sym("y") -> BoundedInteger(Equal(Polynom.fromConstant(4)), typeOf[Int])
    )))(
      Equal(Polynom.fromConstant(8)), LessThan(Polynom.fromConstant(9))
    )(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(GreaterThan(Polynom(Set(t(1, "y"), t(4)))), typeOf[Int]),
      sym("y") -> BoundedInteger(GreaterThan(Polynom.fromConstant(4)), typeOf[Int])
    )))(GreaterThan(Polynom.fromConstant(8)))(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(GreaterThanOrEqual(Polynom(Set(t(1, "y"), t(4)))), typeOf[Int]),
      sym("y") -> BoundedInteger(GreaterThan(Polynom.fromConstant(4)), typeOf[Int])
    )))(GreaterThan(Polynom.fromConstant(8)))(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4)))), typeOf[Int]),
      sym("y") -> BoundedInteger(And(LessThan(Polynom.fromConstant(10)),
                                     GreaterThan(Polynom.fromConstant(0))), typeOf[Int])
    ))) (
      LessThan(Polynom.fromConstant(14)), GreaterThan(Polynom.fromConstant(4))
    )()

  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(4)))),  typeOf[Int]),
      sym("y") -> BoundedInteger(Or(LessThan(Polynom.fromConstant(0)),
                                    GreaterThan(Polynom.fromConstant(10))),  typeOf[Int])
    ))) ()(LessThan(Polynom.fromConstant(4)))

  /**
   * x = y - 5
   * y >= 0 && <= 5
   *
   * x >= -5 && x <= 0
   */
  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(-5)))),  typeOf[Int]),
      sym("y") -> BoundedInteger(And(GreaterThanOrEqual(Polynom.fromConstant(0)),
                                    LessThanOrEqual(Polynom.fromConstant(5))),  typeOf[Int])
    )))(GreaterThanOrEqual(Polynom.fromConstant(-5)))(GreaterThan(Polynom.fromConstant(-5)))

  /**
   * x > y
   * y >= 0 
   *
   * x > y && x > 0
   */
  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(GreaterThan(Polynom(Set(t(1, "y")))),  typeOf[Int]),
      sym("y") -> BoundedInteger(GreaterThanOrEqual(Polynom.fromConstant(0)),  typeOf[Int])
    )))(GreaterThan(Polynom.fromConstant(0)))()

  /**
   * x < y
   * y <= 0 
   *
   * x < y && x < 0
   */
  contextTest(
    new Context(Map(
      sym("x") -> BoundedInteger(LessThan(Polynom(Set(t(1, "y")))),  typeOf[Int]),
      sym("y") -> BoundedInteger(LessThanOrEqual(Polynom.fromConstant(0)),  typeOf[Int])
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
      sym("x") -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(1, "z")))), typeOf[Int]),
      sym("y") -> BoundedInteger(And(GreaterThanOrEqual(Polynom.fromConstant(0)),
                                     LessThanOrEqual(Polynom.fromConstant(5))), typeOf[Int]),
      sym("z") -> BoundedInteger(And(GreaterThanOrEqual(Polynom.fromConstant(1)),
                                      LessThanOrEqual(Polynom.fromConstant(6))),  typeOf[Int])
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
      sym("x") -> BoundedInteger(Equal(Polynom(Set(t(1, "y"), t(1, "z")))),  typeOf[Int]),
      sym("y") -> BoundedInteger(GreaterThanOrEqual(Polynom.fromConstant(0)),  typeOf[Int]),
      sym("z") -> BoundedInteger(GreaterThan(Polynom.fromConstant(1)),  typeOf[Int])
    )))(GreaterThanOrEqual(Polynom.fromSymbol[Int]("z")), GreaterThan(Polynom.fromConstant(1)))()

  def contextTest(c: Context, debug: Boolean = false)(positiveAsserts: Constraint*)(negativeAsserts: Constraint*) {
    test(c.toString) {
      val xBounds = Context.getBoundedInteger("x", typeOf[Int], c).constraint
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
