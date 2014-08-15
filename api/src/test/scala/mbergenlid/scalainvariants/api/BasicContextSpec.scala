package mbergenlid.scalainvariants.api

import mbergenlid.scalainvariants.api.util.TestUniverse
import org.scalatest.FunSuite
import scala.language.implicitConversions

class BasicContextSpec extends FunSuite with TestUniverse {

  import scala.reflect.runtime.universe._

  val variable1 = 1
  val variable2 = 3
  lazy val symbol1 = typeOf[BasicContextSpec].member(newTermName("variable1"))
  lazy val symbol2 = typeOf[BasicContextSpec].member(newTermName("variable2"))

  def chain(symbol: SymbolApi*): SymbolChain[SymbolApi] =
    SymbolChain(symbol.toList)

  implicit def int2Expression(i: Int): Expression = Polynomial.fromConstant(i)
  implicit def sym2Expression(s: SymbolChain[SymbolType]): Expression = Polynomial.fromSymbol[Int](s)

  import Context._

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
    assert(constraint.definitelySubsetOf(GreaterThan(0) || LessThan(-10)), constraint.prettyPrint())

    val context2 = EmptyContext &&
      chain(symbol1) -> GreaterThan(0) ||
      chain(symbol2) -> GreaterThan(1)

    val constraint2 = context2.get(chain(symbol1))
    assert(constraint2 == NoConstraints)
  }

  test("Lookup Context.Or 1") {
    val context = ((EmptyContext && chain(symbol1) -> Equal(20)) &&
      chain(symbol1) -> GreaterThanOrEqual(11)) || (
      chain(symbol1) -> LessThanOrEqual(-1))

    val constraint = context.get(chain(symbol1))
    assert(constraint.definitelySubsetOf(GreaterThanOrEqual(11) || LessThanOrEqual(-1)))
  }

  test("Lookup Context.Or 2") {
    val context = (EmptyContext && chain(symbol1) -> GreaterThan(0) ||
      chain(symbol2) -> GreaterThan(0)) &&
      chain(symbol2) -> LessThanOrEqual(0)

    val constraint = context.get(chain(symbol1))
    assert(constraint.definitelySubsetOf(GreaterThan(0)), constraint.prettyPrint())


    val context2 = (EmptyContext &&
      chain(symbol1) -> GreaterThan(0) &&
      chain(symbol1) -> Equal(chain(symbol1))) &&
      chain(symbol1) -> LessThanOrEqual(0)


    println(context2)
    val constraint2 = context2.get(chain(symbol2))
//    println(constraint2)
  }

  val testClass = new TestClass
  class TestClass {
    val field1: Int = 0
    val field2: Int = 0
    val field3 = new TestClass2
  }
  val testClass2 = new TestClass2
  class TestClass2 {
    val field1: Int = 0
  }
  lazy val testClassSymbol = typeOf[BasicContextSpec].member(newTermName("testClass"))
  lazy val field1Symbol = typeOf[TestClass].member(newTermName("field1"))
  lazy val field2Symbol = typeOf[TestClass].member(newTermName("field2"))
  lazy val field3Symbol = typeOf[TestClass].member(newTermName("field3"))

  lazy val testClass2Symbol = typeOf[BasicContextSpec].member(newTermName("testClass2"))
  lazy val testClass2Field1 = typeOf[TestClass2].member(newTermName("field1"))

  test("Partial SymbolChain") {
    val context = EmptyContext &&
      chain(field1Symbol, testClassSymbol) -> LessThan(10)

    val constraint = context.get(chain(testClassSymbol))
    assert(constraint === PropertyConstraint(field1Symbol, LessThan(10)))

    val context2 = EmptyContext &&
      chain(field1Symbol, testClassSymbol) -> LessThan(10) &&
      chain(field2Symbol, testClassSymbol) -> GreaterThan(0)

    val constraint2 = context2.get(chain(testClassSymbol))
    assert(constraint2.definitelySubsetOf(PropertyConstraint(field1Symbol, LessThan(10)) &&
      PropertyConstraint(field2Symbol, GreaterThan(0))),
      constraint2)
  }

  test("Partial SymbolChain2") {
    val context = EmptyContext &&
      chain(testClass2Field1, field3Symbol, testClassSymbol) -> GreaterThan(0)

    //testClass.field3.field1 > 0
    //testClass -> PropertyConstraint(field3, PropertyConstraint(field1, >0))
    val constraint = context.get(chain(testClassSymbol))
    assert(constraint.definitelySubsetOf(PropertyConstraint(field3Symbol,
      PropertyConstraint(testClass2Field1, GreaterThan(0)))),
      constraint)
  }

  test("Partial SymbolChain3") {
    val context = EmptyContext &&
      chain(testClass2Field1, field3Symbol, testClassSymbol) -> GreaterThan(0) &&
      chain(field1Symbol, testClassSymbol) -> GreaterThan(10)

    val constraint = context.get(chain(testClassSymbol))

    assert(constraint.definitelySubsetOf(
      PropertyConstraint(field3Symbol, PropertyConstraint(testClass2Field1, GreaterThan(0))) &&
      PropertyConstraint(field1Symbol, GreaterThan(10))
    ), constraint.prettyPrint())
  }

  test("Partial SymbolChain4") {
    val context = EmptyContext &&
      chain(testClass2Field1, field3Symbol, testClassSymbol) -> GreaterThan(0) &&
      chain(testClass2Field1, testClass2Symbol) -> LessThan(-10)

    val constraint = context.get(chain(testClassSymbol))
    assert(constraint.definitelySubsetOf(PropertyConstraint(field3Symbol,
      PropertyConstraint(testClass2Field1, GreaterThan(0)))),
      constraint.prettyPrint())

    val constraint2 = context.get(chain(field3Symbol, testClassSymbol))
    assert(constraint2.definitelySubsetOf(PropertyConstraint(testClass2Field1, GreaterThan(0))))
  }

  test("Partial SymbolChain5") {
    val context = EmptyContext &&
      chain(testClassSymbol) -> PropertyConstraint(field1Symbol, GreaterThan(0))

    val constraint = context.get(chain(field1Symbol, testClassSymbol))
    assert(constraint === GreaterThan(0))

    val context2 = EmptyContext &&
      chain(testClassSymbol) ->
        (PropertyConstraint(field1Symbol, GreaterThan(0)) &&
          PropertyConstraint(field2Symbol, LessThan(0)))

    val constraint2 = context2.get(chain(field1Symbol, testClassSymbol))
    assert(constraint2 === GreaterThan(0))
  }

  test("Remove mapping from Context") {
    val context = EmptyContext &&
      chain(symbol1) -> GreaterThan(0) &&
      chain(symbol2) -> LessThan(0)

    val newContext = context - chain(symbol2)
    assert(newContext.get(chain(symbol2)) === NoConstraints)

    val context2 = EmptyContext &&
      chain(field1Symbol, testClassSymbol) -> GreaterThan(0) &&
      chain(symbol1) -> LessThan(100)

    val newContext2 = context2 - chain(testClassSymbol)
    assert(newContext2.get(chain(testClassSymbol)) === NoConstraints)

    val newContext3 = context2 - chain(field1Symbol, testClassSymbol)
    assert(newContext3.get(chain(testClassSymbol)) === NoConstraints)
  }
}
