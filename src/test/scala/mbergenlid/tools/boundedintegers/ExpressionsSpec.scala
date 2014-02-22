
package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

class ExpressionsSpec extends FunSuite 
    with Expressions {

  type BoundedSymbol = String

  implicit def intToConstant(v: Int) = ConstantValue(v)
  implicit def stringToSymbol(s: String) = SymbolExpression(s)
  //  < x + 1    <|  x > 0 && < 10
  //  < x + 1    <|    < 10
  //  x <| 10  
  //  1 <| 10
  //  
  //  (y+5) + 1
  //  
  // ===========================
  //  < x   <|   < 10
  //  < x && < 10
  //  < x + 1 && < 9
  ignore("ArithmeticExpressions") {
    val e1 = Plus(SymbolExpression("x"), ConstantValue(4))
    val e2 = ConstantValue(1)
    val e3 = Plus("y", 5)

    val res1 = e1.substitute("x", e2)
    assert(res1 === ConstantValue(5))
    //val res2 = e1.substitute("x", e3)
    //assert(res2.toString === Plus("y", 9).toString)
    assert((Plus("y", 5) + 1).toString === (Plus("y", 6).toString))
    assert((Plus("y", 5) + Plus("x", 3)) === (Plus(Plus("y", 8), "x")))
  }

  test("Minus arithmetic expressions") {
    val e1 = ConstantValue(4) - ConstantValue(1)
    assert(e1 == ConstantValue(3))

    val e2 = SymbolExpression("x") - ConstantValue(1) - SymbolExpression("x")
    assert(e2 === ConstantValue(-1))

    val e3 = (Plus("y", 5) + Plus("x", 3))
    val e4 = Plus(Plus("y", 8), "x")
    assert(e3 == e4)
  }
}
