
package mbergenlid.tools.boundedintegers

import org.scalatest.FunSuite
import scala.language.implicitConversions
import scala.reflect.runtime.universe._
class ExpressionsSpec extends FunSuite 
    with Expressions {

  type SymbolType = Symbol
  val TypeNothing = typeOf[Nothing]

  val x = 0
  val y = 0
  val z = 0
  implicit def sym(s: String): SymbolType =
    typeOf[this.type].member(newTermName(s))

  implicit def c(v: Int): Expression = Polynom.fromConstant(v)
  implicit def s(s: String) = Polynom.fromSymbol[Int](s)
  def t(v: Int) = Term(ConstantValue(v), Map.empty)
  def t(v: Int, s: String*) = Term(ConstantValue(v), (Map.empty[SymbolType, Int] /: s) { (map, term) =>
    val multiplicity = map.getOrElse(term, 0) + 1
    map + (sym(term) -> multiplicity)
  })

  test("Comparison") {
    assert(c(10) > c(Int.MinValue))
    assert(c(Int.MinValue) < c(10))
    assert(!(c(0) < c(0))) 
  }

  test("Simple addition") {
    val e1 = c(4) + c(5)
    assert(e1 === c(9))

    val e2 = s("x") + c(4)
    assert(e2 === Polynom(t(1, "x"), t(4)))

    val e3 = s("x") + s("y") + c(4)
    assert(e3 === Polynom(t(1, "x"), t(1, "y"), t(4)))
  }

  test("Minus arithmetic expressions") {
    val e1 = c(4) - c(1)
    assert(e1 === c(3))

    val e2 = s("x") - c(1) - s("x")
    assert(e2 === c(-1))

    val e3 = s("y") + c(5) + (s("x") + c(3))
    assert(e3 === Polynom(t(1, "y"), t(1, "x"), t(8)))
  }

  test("Times with constant") {
    val e0 = c(0)
    val e1 = c(1)
    val e2 = c(2)
    val e10 = c(10)
    val x = s("x")

    assert(e2*x === Polynom(Set(t(2, "x"))))
    assert(e0*x === e0)
    assert(e1*x === x)

    assert(e10*e2 === c(20))
    assert(-e1*x === Polynom(Set(t(-1, "x"))))
  }


  test("Times with expression") {
    val e1 = c(4) + s("x")
    val e2 = c(4) + s("y")
    val e3 = s("x")

    assert(c(4)*e1 === Polynom(Set(t(16), t(4, "x"))))
    assert(e1*e2 === Polynom(Set(t(16), t(4, "x"), t(4, "y"), t(1, "x", "y"))))

    assert(e1*e3 === Polynom(Set(t(4, "x"), t(1, "x", "x"))))
  }

  test("Add products") {
    val e1 = Polynom(Set(t(2, "x")))
    val e2 = Polynom(Set(t(4, "y")))

    assert(e1+e2 === Polynom(Set(t(2, "x"), t(4, "y"))))
  }

}
