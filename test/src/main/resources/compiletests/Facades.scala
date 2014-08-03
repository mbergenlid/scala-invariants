package compiletests

import mbergenlid.scalainvariants.annotations.Equal

object Facades {

  def arrayAccessMethod() = {
    val a = Array(1,2,3,4,5)
    a(4) //error

    if(a.length > 5) a(4)
    true
  }

  @Equal(6)
  def basicIntTest() = {
    val y = 5
    y + 1
  }

  @Equal(-5)
  def unaryMinus() = {
    val x = 5
    -x
  }
}