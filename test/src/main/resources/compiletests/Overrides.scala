package compiletests

import mbergenlid.tools.boundedintegers.annotations.{Equal, GreaterThan}

object Overrides {

  trait Super {
    @GreaterThan(0)
    def value: Int
  }

  class Concrete extends Super {
    def value = 10
  }

  def overriddenFieldShouldInheritConstraints() = {
    val s: Super = new Concrete

    //Overridden method value should inherit constraints from super
    @GreaterThan(0)
    val x = new Concrete().value

    @GreaterThan(0)
    val y = s.value //Compile time type is now Super

    x + y
  }

  def overriddenFieldShouldEnforceConstraints() = {
    class Concrete extends Super {
      def value = -1 //error
    }

    class Concrete2 extends Super {
      @Equal(10)
      def value = 10
    }

    val s: Super = new Concrete2

    @Equal(10)
    val x = s.value //error

    @Equal(10)
    val y = s.asInstanceOf[Concrete2].value

    x+y
  }
}