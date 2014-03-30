package mbergenlid.tools.boundedintegers
import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

import scala.language.implicitConversions
class ContextSpec extends FunSuite 
    with TypeContext with BoundedTypeTrees with Expressions {

  type SymbolType = Symbol
  val TypeNothing = typeOf[Nothing]
  val IntSymbol = typeOf[Int].typeSymbol
  def createConstraintFromSymbol(symbol: SymbolType) =
    NoConstraints

  def expressionForType = {
    case TypeRef(_, IntSymbol, Nil) =>
      new ExpressionFactory[Int](typeOf[Int])
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
    val expected =
      LessThan(Polynom.fromConstant(4))
    val c = new Context(Map(
      sym("x") -> expected
    ))

    assert(Context.getConstraint("x", typeOf[Int], c) === expected)
  }

  test("More complex transitive with Mixed") {
    val originalXBound = LessThan(Polynom(Set(t(1, "y"), t(4))))
    val c = new Context(Map(
      sym("x") -> originalXBound,
      sym("y") -> GreaterThan(Polynom.fromConstant(4))
    ))

    val xBounds = Context.getConstraint("x", typeOf[Int], c)
    assert(xBounds === originalXBound)
  }

  contextTest(
    new Context(Map(
      sym("x") -> LessThan(Polynom.fromSymbol[Int]("y")),
      sym("y") -> LessThan(Polynom.fromConstant(4))
    )))(LessThan(Polynom.fromConstant(4)))()

  contextTest(
    new Context(Map(
      sym("x") -> LessThan(Polynom(Set(t(1, "y"), t(4)))) ,
      sym("y") -> LessThan(Polynom.fromConstant(4))
    )))(LessThan(Polynom.fromConstant(8)))(LessThan(Polynom.fromConstant(4)))

  contextTest(
    new Context(Map(
      sym("x") -> LessThan(Polynom.fromSymbol[Int]("y")) ,
      sym("y") -> LessThan(Polynom(Set(t(1, "z"), t(4)))) ,
      sym("z") -> LessThan(Polynom.fromConstant(1))
    )))(LessThan(Polynom.fromConstant(5)))(LessThan(Polynom.fromConstant(4)))

  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynom(Set(t(1, "y"), t(4)))) ,
      sym("y") -> LessThan(Polynom.fromConstant(4))
    )))(LessThan(Polynom.fromConstant(8)))(LessThan(Polynom.fromConstant(4)))

  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynom(Set(t(1, "y"), t(4)))) ,
      sym("y") -> GreaterThan(Polynom.fromConstant(4))
    )))(GreaterThan(Polynom.fromConstant(8)))(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynom(Set(t(1, "y"), t(4)))) ,
      sym("y") -> Equal(Polynom.fromConstant(4))
    )))(
      Equal(Polynom.fromConstant(8)), LessThan(Polynom.fromConstant(9))
    )(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> GreaterThan(Polynom(Set(t(1, "y"), t(4)))) ,
      sym("y") -> GreaterThan(Polynom.fromConstant(4))
    )))(GreaterThan(Polynom.fromConstant(8)))(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> GreaterThanOrEqual(Polynom(Set(t(1, "y"), t(4)))) ,
      sym("y") -> GreaterThan(Polynom.fromConstant(4))
    )))(GreaterThan(Polynom.fromConstant(8)))(GreaterThan(Polynom.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynom(Set(t(1, "y"), t(4)))) ,
      sym("y") -> And(LessThan(Polynom.fromConstant(10)),
                                     GreaterThan(Polynom.fromConstant(0)))
    ))) (
      LessThan(Polynom.fromConstant(14)), GreaterThan(Polynom.fromConstant(4))
    )()

  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynom(Set(t(1, "y"), t(4)))) ,
      sym("y") -> Or(LessThan(Polynom.fromConstant(0)),
                                    GreaterThan(Polynom.fromConstant(10)))
    ))) ()(LessThan(Polynom.fromConstant(4)))

  /**
   * x = y - 5
   * y >= 0 && <= 5
   *
   * x >= -5 && x <= 0
   */
  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynom(Set(t(1, "y"), t(-5)))) ,
      sym("y") -> And(GreaterThanOrEqual(Polynom.fromConstant(0)),
                                    LessThanOrEqual(Polynom.fromConstant(5)))
    )))(GreaterThanOrEqual(Polynom.fromConstant(-5)))(GreaterThan(Polynom.fromConstant(-5)))

  /**
   * x > y
   * y >= 0 
   *
   * x > y && x > 0
   */
  contextTest(
    new Context(Map(
      sym("x") -> GreaterThan(Polynom(Set(t(1, "y")))) ,
      sym("y") -> GreaterThanOrEqual(Polynom.fromConstant(0))
    )))(GreaterThan(Polynom.fromConstant(0)))()

  /**
   * x < y
   * y <= 0 
   *
   * x < y && x < 0
   */
  contextTest(
    new Context(Map(
      sym("x") -> LessThan(Polynom(Set(t(1, "y")))) ,
      sym("y") -> LessThanOrEqual(Polynom.fromConstant(0))
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
      sym("x") -> Equal(Polynom(Set(t(1, "y"), t(1, "z")))),
      sym("y") -> And(GreaterThanOrEqual(Polynom.fromConstant(0)),
                                     LessThanOrEqual(Polynom.fromConstant(5))),
      sym("z") -> And(GreaterThanOrEqual(Polynom.fromConstant(1)),
                                      LessThanOrEqual(Polynom.fromConstant(6)))
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
      sym("x") -> Equal(Polynom(Set(t(1, "y"), t(1, "z")))),
      sym("y") -> GreaterThanOrEqual(Polynom.fromConstant(0)),
      sym("z") -> GreaterThan(Polynom.fromConstant(1))
    )))(GreaterThanOrEqual(Polynom.fromSymbol[Int]("z")), GreaterThan(Polynom.fromConstant(1)))()

  def contextTest(c: Context, debug: Boolean = false)(positiveAsserts: Constraint*)(negativeAsserts: Constraint*) {
    test(c.toString) {
      val xBounds = Context.getConstraint("x", typeOf[Int], c)
      if(debug) {
        println(xBounds.prettyPrint("x"))
      }
      positiveAsserts foreach { x =>
        assert(xBounds obviouslySubsetOf x,
          s"Expected ${xBounds.prettyPrint("x")} to be a subset of ${x.prettyPrint("x")}")
      }
      negativeAsserts.foreach {x =>
        assert(!(xBounds obviouslySubsetOf x),
          s"Expected ${xBounds.prettyPrint("x")} to NOT be a subset of ${x.prettyPrint("x")}")
      }
    }
  }
}
