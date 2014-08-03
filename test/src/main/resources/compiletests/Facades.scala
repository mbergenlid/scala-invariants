package compiletests

import mbergenlid.tools.boundedintegers.annotations.Equal

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
}