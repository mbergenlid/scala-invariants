package mbergenlid.scalainvariants.api.scalacheck

import mbergenlid.scalainvariants.api.util.TestUniverse
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import scala.reflect.runtime.universe.runtimeMirror
import scala.tools.reflect.ToolBox
import scala.language.implicitConversions

class Constraints extends FunSuite with Checkers with TestUniverse {

  implicit def int2Expression(v: Int): Expression = Polynomial.fromConstant(v)

  test("And expression constraints") {
    check(forAll { (sc1: ExpressionConstraint, sc2: ExpressionConstraint) =>
      andProp(sc1, sc2)
    })
  }

  test("And simple constraint to And") {
    check(forAll { (sc1: ExpressionConstraint, sc2: ExpressionConstraint, sc3: ExpressionConstraint) =>
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

      val numGreaterThan = simple match {
        case And(constraints) =>
          constraints.count(sc => sc.isInstanceOf[GreaterThan] || sc.isInstanceOf[GreaterThanOrEqual])
        case _ => 0
      }
      val numLessThan = simple match {
        case And(constraints) =>
          constraints.count(sc => sc.isInstanceOf[LessThan] || sc.isInstanceOf[LessThanOrEqual])
        case _ => 0
      }

      (
        numGreaterThan <= 1 &&
        numLessThan <= 1
      ) :| s"Was $simple"
    })
  }


//  test("Single case") {
////    val arg0 = LessThan(1)
////    val arg1 = GreaterThanOrEqual(-1)
////    assert(!Equal(0).definitelySubsetOf(arg0 && arg1))
////    andProp(arg0, arg1)
//  }

  def testSubset(c1: Constraint, c2: Constraint) = {
    try {
      check(forAll { n: Int =>
        val expr = Equal(Polynomial.fromConstant(n))
        (expr.definitelySubsetOf(c1) ==>
          expr.definitelySubsetOf(c2)) :| s"E"
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
        if(in1 && in2) expr.definitelySubsetOf(and) :| s"In $c1 and $c2, but not in $and"
        else !expr.definitelySubsetOf(and) :| s"In $and"
      })
      true
    } catch {
      case t: Throwable =>
        println(t.getMessage)
        false
    }

  }


  implicit def arbSimpleConstraint = Arbitrary(generateSimpleConstraint)

  val generateSimpleConstraint: Gen[ExpressionConstraint] = for {
    v <- arbitrary[Int]
    sc <- Gen.oneOf(LessThan, LessThanOrEqual, Equal, GreaterThan, GreaterThanOrEqual)
  } yield sc(Polynomial.fromConstant(v))

  implicit def arbAndConstraint = Arbitrary(generateAndConstraint)
  val generateAndConstraint: Gen[And] = for {
    sc <- Gen.nonEmptyListOf(generateSimpleConstraint)
  } yield And(sc)

  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val global = tb.u

}