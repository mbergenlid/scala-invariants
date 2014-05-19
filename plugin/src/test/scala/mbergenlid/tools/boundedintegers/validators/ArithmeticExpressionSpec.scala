package mbergenlid.tools.boundedintegers.validators

import mbergenlid.tools.boundedintegers._
import scala.language.reflectiveCalls

class ArithmeticExpressionSpec extends PluginTestRunner {

  test("Add constant to symbol") {
    compile("""
              |@GreaterThanOrEqual(0)
              |@LessThanOrEqual(5)
              |val x = intBetween0And5
              |
              |val y = x + 4
              |testMethod(y)
              |
              |testMethod(x + 4)
              |testMethod(x + 7)
            """.stripMargin)(List(10))
  }

  test("Subtract constant from symbol") {
    compile("""
              |@GreaterThanOrEqual(0)
              |@LessThanOrEqual(5)
              |val x = intBetween0And5
              |
              |@LessThanOrEqual(5)
              |@GreaterThanOrEqual(-5)
              |val y = x - 5
              |
              |@LessThanOrEqual(1000)
              |@GreaterThanOrEqual(-2)
              |val z = x - 5
              |1
            """.stripMargin)(List(12))
  }

  test("Multiply constant to symbol") {
    compile("""
              |val x = intBetween0And5
              |
              |@GreaterThanOrEqual(0)
              |@LessThanOrEqual(50)
              |val y = x * 10
              |
              |testMethod(x*10)
              |1
            """.stripMargin)(List(8))
  }

  test("Add symbol to symbol") {
    compile("""
              |val x = intBetween0And5
              |
              |@GreaterThanOrEqual(1)
              |@LessThanOrEqual(6)
              |val y = x + 1
              |testMethod(y + x)
              |
              |@GreaterThanOrEqual(1)
              |@LessThanOrEqual(11)
              |val z = x + y
              |1
            """.stripMargin)(List(7))
  }

  test("Subtract symbol from constant") {
    val bounds = expression(
      """
        |val x = intBetween0And5
        |10 - x
      """.stripMargin)

    assertThat(bounds.constraint) definiteSubsetOf cut.LessThan(11)
    assertThat(bounds.constraint).definiteSubsetOf(cut.GreaterThan(4))
    assert(!bounds.constraint.definitelySubsetOf(cut.LessThanOrEqual(5)))
  }

  test("Multiply symbol to constant") {
    val bounds = expression(
      """
        |val x = intBetween0And5
        |2*x
      """.stripMargin)

    assertThat(bounds.constraint) definiteSubsetOf cut.GreaterThanOrEqual(0)
    assertThat(bounds.constraint).definiteSubsetOf(cut.LessThanOrEqual(10))
  }

  test("Add to if expression") {
    val bounds = expression(
      """
        |val x = anotherRandomInteger
        |
        |10 + (if(x < 0) -1 else x)
      """.stripMargin)

    assertThat(bounds.constraint).definiteSubsetOf(cut.Equal(9) || cut.GreaterThanOrEqual(10))
  }

  test("Multiply symbol to other symbol") {
    val bounds = expression(
      """
        |val x = randomInteger
        |val y = anotherRandomInteger
        |
        |x*y
      """.stripMargin)

    import cut._
    assertThat(bounds.constraint).notSubsetOf(And(GreaterThanOrEqual(0), LessThanOrEqual(Int.MaxValue)))
  }

  test("Multiply symbol to same symbol") {
    val bounds = expression(
      """
        |val x = anotherRandomInteger
        |val y = anotherRandomInteger
        |
        |x*x*y
      """.stripMargin)

    import cut._
    val equal = bounds.constraint.find(_.isInstanceOf[Equal])
    assert(equal.isDefined)
    val expr = equal.get.expression
    assert(expr.terms.size == 1)
    assert(expr.terms.head.variables.head._2 == 2)

    assert(expr.toString == "x^2*y")
  }

  test("Multiply bounded symbols") {
    val bounds = expression(
      """
        |val x = intBetween0And5
        |val y = intBetween0And5
        |
        |x*y
      """.stripMargin)

    import cut._
    assertThat(bounds.constraint).definiteSubsetOf(GreaterThanOrEqual(0))
    assertThat(bounds.constraint).definiteSubsetOf(LessThanOrEqual(25))
  }

  /*
   *   == x + 1
   *   <= Overflow
   */
  test("Add constant to symbol should be greater than symbol") {
    compile(
      """
        |val x = anotherRandomInteger
        |val y = anotherRandomInteger
        |
        |def myMethod(@GreaterThanOrEqual(x) in: Int) = false
        |myMethod(x+1) //should fail because x+1 could overflow
        |
        |if(x < 10)
        |  myMethod(x+1)
        |
        |if(x < 10 && y < 10 && y > 0)
        |  myMethod(x - y + 11)
        |
        |if(x < 10 && y > 0 && y < 10)
        |  myMethod(x + y + 10)
        |
        |val z = x + 1
        |true
      """.stripMargin)(List(6))
  }

  test("Subtract constant from symbol should be less than symbol") {
    compile(
      """
        |val x = anotherRandomInteger
        |val y = anotherRandomInteger
        |
        |def myMethod(@LessThanOrEqual(x) in: Int) = false
        |myMethod(x-1) //should fail because x-1 could underflow
        |
        |if(x > 0)
        |  myMethod(x-1)
        |
        |if(x > 0 && y < 10 && y > 0)
        |  myMethod(x - y - 11)
        |
        |if(x > 0 && y > 0 && y < 10)
        |  myMethod(x + y - 10)
        |
        |val z = x - 1
        |true
      """.stripMargin)(List(6))
  }

  test("Should not be able to cancel out methods") {
    compile(
      """
        |val x = anotherRandomInteger
        |val y = anotherRandomInteger
        |var v1 = anotherRandomInteger
        |val x1 = v1
        |val y1 = v1
        |
        |@Equal(1)
        |val a = x - y + 1
        |
        |@Equal(1)
        |val b = x - x + 1
        |
        |@Equal(1)
        |val c = x1 - y1 + 1
        |true
      """.stripMargin)(List(9, 15))
  }

  test("Multiply symbols could overflow") {
    compile(
      """
        |val x = randomInteger
        |val y = anotherRandomInteger
        |
        |@GreaterThanOrEqual(0)
        |val a = x*y
        |
        |false
      """.stripMargin)(List(6))
  }

  test("asd") {
    compile(
      """
        |val x = anotherRandomInteger
        |
        |@GreaterThan(Int.MinValue)
        |val y = x + 1
        |false
      """.stripMargin)(List(5))
  }

  ignore("Constrained by expression") {
    compile(
      """
        |val x = anotherRandomInteger
        |
        |@Equal(x+1)
        |val y = x + 1
        |false
      """.stripMargin)(Nil)
  }
}
