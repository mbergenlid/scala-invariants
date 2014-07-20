
package mbergenlid.scalainvariants.api.expressions

import mbergenlid.scalainvariants.api.SymbolChain
import org.scalatest.FunSuite

import scala.reflect.runtime.universe._

class ExpressionsSpec extends FunSuite {

  var symbolCache = Map[String, SymbolChain]()

  implicit def sym(s: String): SymbolChain = {
    if(!symbolCache.contains(s)) {
      symbolCache += (s -> SymbolChain(List(typeOf[this.type].termSymbol.
        newTermSymbol(newTermName(s), NoPosition, NoFlags | Flag.FINAL ))))
    }
    symbolCache(s)
  }

  implicit def c(v: Int): Expression = Polynomial.fromConstant(v)
  implicit def s(s: String) = Polynomial.fromSymbol[Int](s)
  def t(v: Int) = Term(ConstantValue(v), Map.empty)
  def t(v: Int, s: String*) = Term(ConstantValue(v), (Map.empty[SymbolChain, Int] /: s) { (map, term) =>
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
    assert(e2 === Polynomial(t(1, "x"), t(4)))

    val e3 = s("x") + s("y") + c(4)
    assert(e3 === Polynomial(t(1, "x"), t(1, "y"), t(4)))
  }

  test("Minus arithmetic expressions") {
    val e1 = c(4) - c(1)
    assert(e1 === c(3))

    val e2 = s("x") - c(1) - s("x")
    assert(e2 === c(-1))

    val e3 = s("y") + c(5) + (s("x") + c(3))
    assert(e3 === Polynomial(t(1, "y"), t(1, "x"), t(8)))
  }

  test("Times with constant") {
    val e0 = c(0)
    val e1 = c(1)
    val e2 = c(2)
    val e10 = c(10)
    val x = s("x")

    assert(e2*x === Polynomial(Set(t(2, "x"))))
    assert(e0*x === e0)
    assert(e1*x === x)

    assert(e10*e2 === c(20))
    assert(-e1*x === Polynomial(Set(t(-1, "x"))))
  }


  test("Times with expression") {
    val e1 = c(4) + s("x")
    val e2 = c(4) + s("y")
    val e3 = s("x")

    assert(c(4)*e1 === Polynomial(Set(t(16), t(4, "x"))))
    assert(e1*e2 === Polynomial(Set(t(16), t(4, "x"), t(4, "y"), t(1, "x", "y"))))

    assert(e1*e3 === Polynomial(Set(t(4, "x"), t(1, "x", "x"))))
  }

  test("Add products") {
    val e1 = Polynomial(Set(t(2, "x")))
    val e2 = Polynomial(Set(t(4, "y")))

    assert(e1+e2 === Polynomial(Set(t(2, "x"), t(4, "y"))))
  }

  test("Substitute 0-n => 0-Int.MinValue") {
    val e1 = Polynomial(Set(t(-1, "n")))
    val e2 = Polynomial.fromConstant(Int.MinValue)

    val result = e1.substitute("n", e2)
    assert(result > Polynomial.fromConstant(Int.MaxValue))
  }

  test("Constant comparison") {
    assert(Polynomial.fromConstant(Int.MinValue) < Polynomial.fromConstant(0))
    assert(Polynomial.fromConstant(0) > Polynomial.fromConstant(Int.MinValue+1))
    assert(!(Polynomial.fromConstant(0) < Polynomial.fromConstant(Int.MinValue)))
    assert(Polynomial.fromConstant(0) > Polynomial.fromConstant(Int.MinValue))
  }

  test("Comparison with Int.MinValue") {
    val diff1 = Polynomial(Set(t(1, "x"))) -
      Polynomial(Set(t(1, "x"), t(Int.MinValue)))

    //x - (-10 + x) = 10
    //x - (x - 10) = 10
    val diff2 = Polynomial(Set(t(1, "x"))) -
      Polynomial(Set(t(Int.MinValue), t(1, "x")))

    assert(diff1 > Polynomial.fromConstant(Int.MaxValue))
    assert(diff2 > Polynomial.fromConstant(Int.MaxValue))
  }
}
