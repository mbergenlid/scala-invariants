package mbergenlid.scalainvariants.api

import mbergenlid.scalainvariants.api.util.TestUniverse
import org.scalatest.FunSuite

import scala.language.implicitConversions

class ContextSpec extends FunSuite with TestUniverse {
  import scala.reflect.runtime.universe._

  val transitiveContext = TransitiveContext

  class Wrapper {
    val x: Int = 1
    val y: Int = 2
    val z: Int = 3
  }

  var symbolCache = Map[String, SymbolChain[SymbolType]](
    "x" -> SymbolChain(List(typeOf[Wrapper].member(newTermName("x")))),
    "y" -> SymbolChain(List(typeOf[Wrapper].member(newTermName("y")))),
    "z" -> SymbolChain(List(typeOf[Wrapper].member(newTermName("z"))))
  )

  implicit def int2Expression(i: Int): Expression = Polynomial.fromConstant(i)
  implicit def sym(s: String): SymbolChain[SymbolType] = {
//    if(!symbolCache.contains(s)) {
//      symbolCache += (s -> SymbolChain[SymbolType](List(typeOf[this.type].termSymbol.
//        newTermSymbol(newTermName(s), NoPosition, NoFlags | Flag.FINAL ))))
//    }
    symbolCache(s)
  }
  implicit def sym2Expression(sym: SymbolChain[SymbolType]): Expression = Polynomial.fromSymbol[Int](sym)

  def t(v: Int) = Term(ConstantValue(v), Map.empty)
  def t(v: Int, s: String*) = Term(ConstantValue(v), (Map.empty[SymbolChain[SymbolType], Int] /: s) { (map, term) =>
    val multiplicity = map.getOrElse(sym(term), 0) + 1
    map + (sym(term) -> multiplicity)
  })

  import Context._
  test("Simple retrieval") {
    val expected =
      LessThan(Polynomial.fromConstant(4))
    val c = EmptyContext && sym("x") -> expected

    assert(transitiveContext.getConstraint("x", c) === expected)
  }

  test("More complex transitive with Mixed") {
    val originalXBound = LessThan(Polynomial(Set(t(1, "y"), t(4))))
    val c = EmptyContext &&
      sym("x") -> originalXBound &&
      sym("y") -> GreaterThan(Polynomial.fromConstant(4))


    val xBounds = transitiveContext.getConstraint("x", c)
    assert(xBounds.asInstanceOf[And].constraints.head === originalXBound)
  }

  contextTest(
    Map(
      sym("x") -> LessThan(Polynomial.fromSymbol[Int]("y")),
      sym("y") -> LessThan(Polynomial.fromConstant(4))
    ))(LessThan(Polynomial.fromConstant(4)))()

  contextTest(
    Map(
      sym("x") -> LessThan(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> LessThan(Polynomial.fromConstant(4))
    ))(LessThan(Polynomial.fromConstant(8)))(LessThan(Polynomial.fromConstant(4)))

  contextTest(
    Map(
      sym("x") -> LessThan(Polynomial.fromSymbol[Int]("y")) ,
      sym("y") -> LessThan(Polynomial(Set(t(1, "z"), t(4)))) ,
      sym("z") -> LessThan(Polynomial.fromConstant(1))
    ))(LessThan(Polynomial.fromConstant(5)))(LessThan(Polynomial.fromConstant(4)))

  contextTest(
    Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> LessThan(Polynomial.fromConstant(4))
    ))(LessThan(Polynomial.fromConstant(8)))(LessThan(Polynomial.fromConstant(4)))

  contextTest(
    Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> GreaterThan(Polynomial.fromConstant(4))
    ))(GreaterThan(Polynomial.fromConstant(8)))(GreaterThan(Polynomial.fromConstant(9)))

  contextTest(
    Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> Equal(Polynomial.fromConstant(4))
    ))(
      Equal(Polynomial.fromConstant(8)), LessThan(Polynomial.fromConstant(9))
    )(GreaterThan(Polynomial.fromConstant(9)))

  contextTest(
    Map(
      sym("x") -> GreaterThan(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> GreaterThan(Polynomial.fromConstant(4))
    ))(GreaterThan(Polynomial.fromConstant(8)))(GreaterThan(Polynomial.fromConstant(9)))

  contextTest(
    Map(
      sym("x") -> GreaterThanOrEqual(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> GreaterThan(Polynomial.fromConstant(4))
    ))(GreaterThan(Polynomial.fromConstant(8)))(GreaterThan(Polynomial.fromConstant(9)))

  contextTest(
    Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> And(LessThan(Polynomial.fromConstant(10)),
        GreaterThan(Polynomial.fromConstant(0)))
    )) (
    LessThan(Polynomial.fromConstant(14)), GreaterThan(Polynomial.fromConstant(4))
  )()

  contextTest(
    Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(4)))),
      sym("y") -> (LessThan(Polynomial.fromConstant(0)) ||
        GreaterThan(Polynomial.fromConstant(10)))
    )) ()(LessThan(Polynomial.fromConstant(4)))

  /**
   * x = y - 5
   * y >= 0 && <= 5
   *
   * x >= -5 && x <= 0
   */
  contextTest(
    Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(-5)))) ,
      sym("y") -> And(GreaterThanOrEqual(Polynomial.fromConstant(0)),
        LessThanOrEqual(Polynomial.fromConstant(5)))
    ))(GreaterThanOrEqual(Polynomial.fromConstant(-5)))(GreaterThan(Polynomial.fromConstant(-5)))

  /**
   * x > y
   * y >= 0 
   *
   * x > y && x > 0
   */
  contextTest(
    Map(
      sym("x") -> GreaterThan(Polynomial(Set(t(1, "y")))) ,
      sym("y") -> GreaterThanOrEqual(Polynomial.fromConstant(0))
    ))(GreaterThan(Polynomial.fromConstant(0)))()

  /**
   * x < y
   * y <= 0 
   *
   * x < y && x < 0
   */
  contextTest(
    Map(
      sym("x") -> LessThan(Polynomial(Set(t(1, "y")))) ,
      sym("y") -> LessThanOrEqual(Polynomial.fromConstant(0))
    ))(LessThan(Polynomial.fromConstant(0)))()

  /**
   * _ = y + z
   * y >= 0 && y <= 5
   * z >= 1 && z <= 6
   *
   * _ >= 1
   */
  contextTest(
    Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(1, "z")))),
      sym("y") -> And(GreaterThanOrEqual(Polynomial.fromConstant(0)),
        LessThanOrEqual(Polynomial.fromConstant(5))),
      sym("z") -> And(GreaterThanOrEqual(Polynomial.fromConstant(1)),
        LessThanOrEqual(Polynomial.fromConstant(6)))
    ))(GreaterThanOrEqual(Polynomial.fromConstant(1)), LessThan(Polynomial.fromConstant(12)))(LessThanOrEqual(Polynomial.fromConstant(10)))

  /**
   * _ = y + z
   * y >= 0 && y <= 5
   * z >= 1 && z <= 6
   *
   * _ >= 1
   */
  contextTest(
    Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(1, "z")))),
      sym("y") -> GreaterThanOrEqual(Polynomial.fromConstant(0)),
      sym("z") -> GreaterThan(Polynomial.fromConstant(1))
    ))(GreaterThanOrEqual(Polynomial.fromSymbol[Int]("z")), GreaterThan(Polynomial.fromConstant(1)))()

  contextConstantsTest(
    EmptyContext &&
      sym("x") -> GreaterThan(10),
    Equal(0),
    Set(sym("x"))
  )(LessThan(sym("x")))()

  contextConstantsTest(
    EmptyContext &&
      sym("x") -> Equal(20) &&
      (Context.apply(sym("x") -> GreaterThan(10)) ||
        sym("x") -> LessThan(0)),
    Equal(0),
    Set(sym("x"))
  )(LessThan(sym("x")))()

  def contextTest
      (contextMap: Map[SymbolChain[SymbolType], Constraint], debug: Boolean = false)
      (positiveAsserts: Constraint*)
      (negativeAsserts: Constraint*) {
    val c = contextMap.foldLeft[Context](EmptyContext)(_&&_)
    test(c.toString) {
      val xBounds = transitiveContext.getConstraint("x", c)
      if(debug) {
        println(xBounds.prettyPrint("x"))
      }
      positiveAsserts foreach { x =>
        assert(xBounds definitelySubsetOf x,
          s"Expected ${xBounds.prettyPrint("x")} to be a subset of ${x.prettyPrint("x")}")
      }
      negativeAsserts.foreach {x =>
        assert(!(xBounds definitelySubsetOf x),
          s"Expected ${xBounds.prettyPrint("x")} to NOT be a subset of ${x.prettyPrint("x")}")
      }
    }
  }


  def contextConstantsTest(
    context: Context,
    startConstraint: Constraint,
    symbols: Set[SymbolChain[SymbolType]],
    debug: Boolean = false)(
    positiveAsserts: Constraint*)(
    negativeAsserts: Constraint*) {

      val c = context
      test("Constants: " + c.toString) {
        val bounds = transitiveContext.substituteConstants(startConstraint, symbols, typeOf[Int], c)
        if(debug) {
          println(bounds.prettyPrint())
        }
        positiveAsserts foreach { x =>
          assert(bounds definitelySubsetOf x,
            s"Expected ${bounds.prettyPrint("_")} to be a subset of ${x.prettyPrint("_")}")
        }
        negativeAsserts.foreach {x =>
          assert(!(bounds definitelySubsetOf x),
            s"Expected ${bounds.prettyPrint("_")} to NOT be a subset of ${x.prettyPrint("_")}")
        }
      }
  }
}
