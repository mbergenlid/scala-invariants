package compiletests

import mbergenlid.scalainvariants.annotations.{GreaterThan, Equal}

object BoundToInput {

  def boundToSimpleInput() = {
    @Equal("n")
    def myMethod1(n: Int) = n

    @Equal("n")
    def myMethod2(n: Int) = n + 1 //error
  }

  def useInputBoundMethod() = {
    @Equal("n")
    def myMethod(n: Int) = n

    @Equal(10)
    val x = myMethod(10)
    x
  }

  def boundToDoubleInput() = {
    @Equal("n")
    def myMethod1(n: Double) = n

    @Equal("n")
    def myMethod2(n: Double) = n + 1 //error
  }

  def boundInputToOtherInput() = {
    def myMethod(from: Int, @GreaterThan("from") to: Int) = to - from

    myMethod(1,2)
    myMethod(2,1) //error
  }
}