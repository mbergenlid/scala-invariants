package mbergenlid.tools.boundedintegers
import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

import scala.language.implicitConversions

class ContextSpec extends FunSuite
    with TypeContext with Constraints with Expressions {

  type RealSymbolType = Symbol
  val TypeNothing = typeOf[Nothing]
  val IntSymbol = typeOf[Int].typeSymbol
  def createConstraintFromSymbol(symbol: SymbolType) =
    NoConstraints

  val OverflowConstant = Polynomial(Set(Term(TypedConstantValue[Int](BigDecimal(Int.MaxValue+1)), Map.empty)))

  def expressionForType = {
    case TypeRef(_, IntSymbol, Nil) =>
      new ExpressionFactory[Int](typeOf[Int])
  }

  var symbolCache = Map[String, SymbolType]()

  implicit def sym(s: String): SymbolType = {
    if(!symbolCache.contains(s)) {
      symbolCache += (s -> SymbolChain(List(typeOf[this.type].termSymbol.
        newTermSymbol(newTermName(s), NoPosition, NoFlags | Flag.FINAL ))))
    }
    symbolCache(s)
  }

  def t(v: Int) = Term(ConstantValue(v), Map.empty)
  def t(v: Int, s: String*) = Term(ConstantValue(v), (Map.empty[SymbolType, Int] /: s) { (map, term) =>
    val multiplicity = map.getOrElse(sym(term), 0) + 1
    map + (sym(term) -> multiplicity)
  })

  test("Simple retrieval") {
    val expected =
      LessThan(Polynomial.fromConstant(4))
    val c = new Context(Map(
      sym("x") -> expected
    ))

    assert(Context.getConstraint("x", typeOf[Int], c) === expected)
  }

  test("More complex transitive with Mixed") {
    val originalXBound = LessThan(Polynomial(Set(t(1, "y"), t(4))))
    val c = new Context(Map(
      sym("x") -> originalXBound,
      sym("y") -> GreaterThan(Polynomial.fromConstant(4))
    ))

    val xBounds = Context.getConstraint("x", typeOf[Int], c)
    assert(xBounds.asInstanceOf[And].constraints.head === originalXBound)
  }

  test("Failed case") {
    val res = for {
      ec1 <- Equal(Polynomial.fromSymbol[Int](sym("x")))
      ec2 <- GreaterThanOrEqual(Polynomial.fromConstant(0)) && LessThan(Polynomial.fromConstant(10)) &&
        Equal(Polynomial.fromSymbol[Int](sym("x")))
      s <- ec1.substitute(sym("x"), ec2)
    } yield s
  }



  contextTest(
    new Context(Map(
      sym("x") -> LessThan(Polynomial.fromSymbol[Int]("y")),
      sym("y") -> LessThan(Polynomial.fromConstant(4))
    )))(LessThan(Polynomial.fromConstant(4)))()

  contextTest(
    new Context(Map(
      sym("x") -> LessThan(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> LessThan(Polynomial.fromConstant(4))
    )))(LessThan(Polynomial.fromConstant(8)))(LessThan(Polynomial.fromConstant(4)))

  contextTest(
    new Context(Map(
      sym("x") -> LessThan(Polynomial.fromSymbol[Int]("y")) ,
      sym("y") -> LessThan(Polynomial(Set(t(1, "z"), t(4)))) ,
      sym("z") -> LessThan(Polynomial.fromConstant(1))
    )))(LessThan(Polynomial.fromConstant(5)))(LessThan(Polynomial.fromConstant(4)))

  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> LessThan(Polynomial.fromConstant(4))
    )))(LessThan(Polynomial.fromConstant(8)))(LessThan(Polynomial.fromConstant(4)))

  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> GreaterThan(Polynomial.fromConstant(4))
    )))(GreaterThan(Polynomial.fromConstant(8)))(GreaterThan(Polynomial.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> Equal(Polynomial.fromConstant(4))
    )))(
      Equal(Polynomial.fromConstant(8)), LessThan(Polynomial.fromConstant(9))
    )(GreaterThan(Polynomial.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> GreaterThan(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> GreaterThan(Polynomial.fromConstant(4))
    )))(GreaterThan(Polynomial.fromConstant(8)))(GreaterThan(Polynomial.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> GreaterThanOrEqual(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> GreaterThan(Polynomial.fromConstant(4))
    )))(GreaterThan(Polynomial.fromConstant(8)))(GreaterThan(Polynomial.fromConstant(9)))

  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(4)))) ,
      sym("y") -> And(LessThan(Polynomial.fromConstant(10)),
                                     GreaterThan(Polynomial.fromConstant(0)))
    ))) (
      LessThan(Polynomial.fromConstant(14)), GreaterThan(Polynomial.fromConstant(4))
    )()

  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(4)))),
      sym("y") -> (LessThan(Polynomial.fromConstant(0)) ||
                    GreaterThan(Polynomial.fromConstant(10)))
    ))) ()(LessThan(Polynomial.fromConstant(4)))

  /**
   * x = y - 5
   * y >= 0 && <= 5
   *
   * x >= -5 && x <= 0
   */
  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(-5)))) ,
      sym("y") -> And(GreaterThanOrEqual(Polynomial.fromConstant(0)),
                                    LessThanOrEqual(Polynomial.fromConstant(5)))
    )))(GreaterThanOrEqual(Polynomial.fromConstant(-5)))(GreaterThan(Polynomial.fromConstant(-5)))

  /**
   * x > y
   * y >= 0 
   *
   * x > y && x > 0
   */
  contextTest(
    new Context(Map(
      sym("x") -> GreaterThan(Polynomial(Set(t(1, "y")))) ,
      sym("y") -> GreaterThanOrEqual(Polynomial.fromConstant(0))
    )))(GreaterThan(Polynomial.fromConstant(0)))()

  /**
   * x < y
   * y <= 0 
   *
   * x < y && x < 0
   */
  contextTest(
    new Context(Map(
      sym("x") -> LessThan(Polynomial(Set(t(1, "y")))) ,
      sym("y") -> LessThanOrEqual(Polynomial.fromConstant(0))
    )))(LessThan(Polynomial.fromConstant(0)))()

  /**
   * _ = y + z
   * y >= 0 && y <= 5
   * z >= 1 && z <= 6
   *
   * _ >= 1
   */
  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(1, "z")))),
      sym("y") -> And(GreaterThanOrEqual(Polynomial.fromConstant(0)),
                                     LessThanOrEqual(Polynomial.fromConstant(5))),
      sym("z") -> And(GreaterThanOrEqual(Polynomial.fromConstant(1)),
                                      LessThanOrEqual(Polynomial.fromConstant(6)))
    )))(GreaterThanOrEqual(Polynomial.fromConstant(1)), LessThan(Polynomial.fromConstant(12)))(LessThanOrEqual(Polynomial.fromConstant(10)))
  
  /**
   * _ = y + z
   * y >= 0 && y <= 5
   * z >= 1 && z <= 6
   *
   * _ >= 1
   */
  contextTest(
    new Context(Map(
      sym("x") -> Equal(Polynomial(Set(t(1, "y"), t(1, "z")))),
      sym("y") -> GreaterThanOrEqual(Polynomial.fromConstant(0)),
      sym("z") -> GreaterThan(Polynomial.fromConstant(1))
    )))(GreaterThanOrEqual(Polynomial.fromSymbol[Int]("z")), GreaterThan(Polynomial.fromConstant(1)))()

  def contextTest(c: Context, debug: Boolean = false)(positiveAsserts: Constraint*)(negativeAsserts: Constraint*) {
    test(c.toString) {
      val xBounds = Context.getConstraint("x", typeOf[Int], c)
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
}
