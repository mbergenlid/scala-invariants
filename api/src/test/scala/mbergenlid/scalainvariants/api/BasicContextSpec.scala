package mbergenlid.scalainvariants.api

import mbergenlid.scalainvariants.api.constraints.{NoConstraints, GreaterThan, LessThan}
import mbergenlid.scalainvariants.api.expressions.{Polynomial, Expression}
import org.scalatest.FunSuite
import scala.language.implicitConversions

class BasicContextSpec extends FunSuite {

  import scala.reflect.runtime.universe._

  val variable1 = 1
  val variable2 = 3
  lazy val symbol1 = typeOf[BasicContextSpec].member(newTermName("variable1"))
  lazy val symbol2 = typeOf[BasicContextSpec].member(newTermName("variable2"))

  val testClass = new TestClass
  class TestClass {
    val field1: Int = _
    val field2: Int = _
  }
  lazy val testClassSymbol = typeOf[BasicContextSpec].member(newTermName("testClass"))
  lazy val field1Symbol = typeOf[TestClass].member(newTermName("field1"))
  lazy val field2Symbol = typeOf[TestClass].member(newTermName("field2"))

  def chain(symbol: SymbolApi): SymbolChain =
    SymbolChain(List(symbol))

  implicit def int2Expression(i: Int): Expression = Polynomial.fromConstant(i)

  test("Basic retrieval") {
    val context = EmptyContext &&
      chain(symbol1) -> LessThan(1) &&
      chain(symbol1) -> GreaterThan(-10) &&
      chain(symbol2) -> GreaterThan(0)

    val constraint = context.get(chain(symbol1))
    assert(constraint definitelySubsetOf (LessThan(1) && GreaterThan(-10)), constraint.prettyPrint())
  }

  test("Context.Or") {
    val context = EmptyContext &&
      chain(symbol1) -> GreaterThan(0) &&
      chain(symbol2) -> GreaterThan(1) ||
      chain(symbol1) -> LessThan(-10)

    val constraint = context.get(chain(symbol1))
    assert(constraint.definitelySubsetOf(GreaterThan(0) || LessThan(-10)))

    val context2 = EmptyContext &&
      chain(symbol1) -> GreaterThan(0) ||
      chain(symbol2) -> GreaterThan(1)

    val constraint2 = context2.get(chain(symbol1))
    assert(constraint2 == NoConstraints)
  }

}
