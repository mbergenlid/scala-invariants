package mbergenlid.tools.boundedintegers.scalacheck

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import mbergenlid.tools.boundedintegers.{Constraints => CUT}
import scala.reflect.runtime.universe.runtimeMirror
import scala.tools.reflect.ToolBox

class Constraints extends FunSuite with Checkers with CUT {

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

  def andProp(c1: Constraint, c2: Constraint) = {
    val and = c1 && c2
    forAll { n: Int =>
      val expr = Equal(Polynomial.fromConstant(n))
      val in1 = expr.definitelySubsetOf(c1)
      val in2 = expr.definitelySubsetOf(c2)
      if(in1 && in2) expr.definitelySubsetOf(and)
      else !expr.definitelySubsetOf(and)
    }
  }

  implicit def arbSimpleConstraint = Arbitrary(generateSimpleConstraint)

  val generateSimpleConstraint: Gen[SimpleConstraint] = for {
    v <- arbitrary[Int]
    sc <- Gen.oneOf(LessThan, LessThanOrEqual, Equal, GreaterThan, GreaterThanOrEqual)
  } yield sc(Polynomial.fromConstant(v))

  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val global = tb.u
  import global._

  override val TypeNothing: TypeType = typeOf[Nothing]
  override type RealSymbolType = global.Symbol
}



