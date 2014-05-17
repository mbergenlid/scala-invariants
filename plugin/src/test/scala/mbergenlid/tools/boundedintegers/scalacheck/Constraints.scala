package mbergenlid.tools.boundedintegers.scalacheck

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import mbergenlid.tools.boundedintegers.{Constraints => CUT}
import scala.reflect.runtime.universe.runtimeMirror
import scala.tools.reflect.ToolBox
import scala.language.implicitConversions

class Constraints extends FunSuite with Checkers with CUT {

  implicit def int2Expression(v: Int): Expression = Polynomial.fromConstant(v)

  test("And simple constraints") {
    check(forAll { (sc1: SimpleConstraint, sc2: SimpleConstraint) =>
      andProp(sc1, sc2)
    })
  }

  test("And simple constraint to And") {
    check(forAll { (sc1: SimpleConstraint, sc2: SimpleConstraint, sc3: SimpleConstraint) =>
      andProp(sc1 && sc2, sc3)
    })
  }

  test("And") {
    check(forAll { (a1: And, a2: And) =>
       andProp(a1, a2)
    })
  }

  test("And definitelySubsetOf itself") {
    check(forAll { a: And =>
      a.definitelySubsetOf(a)
    })
  }

  test("And simplify") {
    check(forAll { a: And =>
      val simple = a.simplify()
      val numGreaterThan =
        simple.constraints.count(sc => sc.isInstanceOf[GreaterThan] || sc.isInstanceOf[GreaterThanOrEqual])
      val numLessThan =
        simple.constraints.count(sc => sc.isInstanceOf[LessThan] || sc.isInstanceOf[LessThanOrEqual])

      (
        numGreaterThan <= 1 &&
        numLessThan <= 1
      ) :| s"Was $simple"
    })
  }

  test("And definitelySubsetOf And") {
    check(forAll { (a1: And, a2: And) =>
      val simpleA1 = a1.simplify()
      val simpleA2 = a2.simplify()
      val subset = simpleA1.definitelySubsetOf(simpleA2)
      if(subset) testSubset(simpleA1, simpleA2) :| s"$simpleA1 :: $simpleA2"
      else true :| "Asd"
    })
  }

  test("And lower/upper bound") {
    check(forAll { a: And =>
      val simplified = a.simplify()
      forAll(choose(simplified)) { n =>
        Equal(n).definitelySubsetOf(simplified) :| s"${simplified.upperBoundInclusive}"
      }
    })
  }

  test("Single case") {
    val arg0 = And(List(Equal(1))).simplify()
    val arg1 = And(List(GreaterThan(-472153321))).simplify()
    assert(arg0.definitelySubsetOf(arg1))
    testSubset(arg0, arg1)
  }

  def testSubset(c1: Constraint, c2: Constraint) = {
    try {
      check(forAll { n: Int =>
        val expr = Equal(Polynomial.fromConstant(n))
        expr.definitelySubsetOf(c1) ==>
          expr.definitelySubsetOf(c2)
      })
      true
    } catch {
      case e: Throwable => println(e.getMessage); false
    }
  }

  def andProp(c1: Constraint, c2: Constraint) = {
    val and = c1 && c2
    try {
      check(forAll { n: Int =>
        val expr = Equal(Polynomial.fromConstant(n))
        val in1 = expr.definitelySubsetOf(c1)
        val in2 = expr.definitelySubsetOf(c2)
        if(in1 && in2) expr.definitelySubsetOf(and)
        else !expr.definitelySubsetOf(and)
      })
      true
    } catch {
      case _: Throwable => false
    }

  }

  def choose(a: And) = {
    val l: Int =
      a.lowerBoundInclusive.find(_.v.isConstant).
        map(_.v.asConstant.value.toInt).getOrElse(Int.MinValue)
    val u = a.upperBoundInclusive.find(_.v.isConstant).
      map(_.v.asConstant.value.toInt).getOrElse(Int.MaxValue)

    Gen.choose(l, u)
  }


  implicit def arbSimpleConstraint = Arbitrary(generateSimpleConstraint)

  val generateSimpleConstraint: Gen[SimpleConstraint] = for {
    v <- arbitrary[Int]
    sc <- Gen.oneOf(LessThan, LessThanOrEqual, Equal, GreaterThan, GreaterThanOrEqual)
  } yield sc(Polynomial.fromConstant(v))

  implicit def arbAndConstraint = Arbitrary(generateAndConstraint)
  val generateAndConstraint: Gen[And] = for {
    sc <- Gen.nonEmptyListOf(generateSimpleConstraint)
  } yield And(sc)

  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val global = tb.u
  import global._

  override val TypeNothing: TypeType = typeOf[Nothing]
  override type RealSymbolType = global.Symbol
}



