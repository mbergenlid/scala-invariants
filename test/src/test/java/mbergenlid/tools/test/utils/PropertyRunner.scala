package mbergenlid.tools.test.utils

import scala.reflect.runtime.universe
import mbergenlid.tools.boundedintegers.annotations.{RichNumeric, Bounded}
import org.scalacheck.Prop._
import mbergenlid.tools.boundedintegers.MyUniverse
import scala.reflect.api.Universe
import org.scalacheck.Test
import org.scalatest.Assertions
import org.scalacheck.util.ConsoleReporter

object PropertyRunner extends MyUniverse with Assertions {

  val global: Universe = universe
  import universe._

  def execute(className: String): Unit = {
    val m = runtimeMirror(getClass.getClassLoader)
    val module = m.staticModule(className)

    val instance = m.reflectModule(module).instance
    val instanceMirror = m.reflect(instance)
    for {
      m <- module.typeSignature.members
      if m.isMethod && m.annotations.exists(_.tpe <:< typeOf[Bounded])
    } executeMethod(instanceMirror, m.asMethod)
  }

  private def executeMethod(instanceMirror: InstanceMirror, method: MethodSymbol): Unit = {

    val param = method.paramss.head.head
    if(param.typeSignature =:= typeOf[Int] && method.returnType =:= typeOf[Int]) {
      import org.scalacheck.Prop._

      val constraint: Constraint = methodConstraints(method)

      val prop = forAll(property[Int, Int](instanceMirror.reflectMethod(method.asMethod), constraint))
      val result = Test.check(prop) (_.withTestCallback(ConsoleReporter(1)))
      assert(result.passed)
    }
  }

  private def methodConstraints(method: MethodSymbol) = {
    val globalSymbol = method.asInstanceOf[global.MethodSymbol]
    BoundsFactory.apply(globalSymbol, globalSymbol.returnType)
  }

  import org.scalacheck.Prop
  private def property[T, R: RichNumeric: TypeTag]
            (method: MethodMirror, constraint: Constraint): (T => Prop) = { p: T =>

    val res = method.apply(p).asInstanceOf[R]
    Equal(Polynomial.fromConstant(res)).obviouslySubsetOf(constraint)
  }
}
