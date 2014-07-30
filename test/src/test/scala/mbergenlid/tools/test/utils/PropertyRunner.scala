package mbergenlid.tools.test.utils

import scala.reflect.runtime.universe
import mbergenlid.tools.boundedintegers.annotations._

import org.scalacheck._
import Prop._
import Arbitrary._
import Shrink._
import org.scalacheck.util.Pretty
import Pretty._
import mbergenlid.tools.boundedintegers.MyUniverse
import scala.reflect.api.Universe
import org.scalacheck.Test
import org.scalatest.Assertions
import org.scalacheck.util.ConsoleReporter

object PropertyRunner extends MyUniverse with Assertions {

  val global: Universe = universe
  import universe._

  override lazy val IntType = global.typeOf[Int]
  override lazy val BoundedAnnotationType = universe.typeOf[Bounded].asInstanceOf[global.Type]
  override lazy val GreaterThanOrEqualType = universe.typeOf[GreaterThanOrEqual].asInstanceOf[global.Type]
  override lazy val GreaterThanType = universe.typeOf[GreaterThan].asInstanceOf[global.Type]
  override lazy val EqualType = universe.typeOf[Equal].asInstanceOf[global.Type]
  override lazy val LessThanOrEqualType = universe.typeOf[LessThanOrEqual].asInstanceOf[global.Type]
  override lazy val LessThanType = universe.typeOf[LessThan].asInstanceOf[global.Type]
  override lazy val ThisSymbol =
    universe.typeOf[this.type].termSymbol.newTermSymbol(newTermName("this")).asInstanceOf[SymbolType]

  def execute(className: String): Unit = {
    val m = runtimeMirror(getClass.getClassLoader)
    val module = m.staticModule(className)

    val instance = m.reflectModule(module).instance
    val instanceMirror = m.reflect(instance)
    val properties = new Properties(className)

    for {
      m <- module.typeSignature.members
      if m.isMethod && m.annotations.exists(_.tpe <:< typeOf[Bounded] &&
        m.asMethod.returnType =:= typeOf[Int])
    } {
      properties.property(m.name.toString) = propertyForMethod(instanceMirror, m.asMethod)
    }

    val result = Test.checkProperties(
      Test.Parameters.default.withTestCallback(ConsoleReporter(1)), properties)

    assert(result.forall(_._2.passed))
  }

  private def propertyForMethod(instanceMirror: InstanceMirror, method: MethodSymbol): Prop = {
    val constraint: Constraint = methodConstraints(method)
    method.paramss match {
      case parameterSymbols :: _ =>
        if(parameterSymbols.size <= 8) {
          val params = parameterSymbols.map(p => getPropertyParams(p))

          val propertyConstructor = params match {
            case a1::Nil => property[Int](a1) _
            case a1::a2::Nil => property[Int](a1,a2) _
            case a1::a2::a3::Nil => property[Int](a1,a2,a3) _
            case a1::a2::a3::a4::Nil => property[Int](a1,a2,a3,a4) _
            case a1::a2::a3::a4::a5::Nil => property[Int](a1,a2,a3,a4,a5) _
            case a1::a2::a3::a4::a5::a6::Nil => property[Int](a1,a2,a3,a4,a5,a6) _
            case a1::a2::a3::a4::a5::a6::a7::Nil => property[Int](a1,a2,a3,a4,a5,a6,a7) _
            case a1::a2::a3::a4::a5::a6::a7::a8::Nil => property[Int](a1,a2,a3,a4,a5,a6,a7,a8) _
            case _ => throw new RuntimeException("Should not happen")
          }

          propertyConstructor(instanceMirror.reflectMethod(method.asMethod), constraint)
        } else {
          Prop.exception(new IllegalArgumentException("Doesn't support more than 8 parameters"))
        }
      case Nil =>
        val res = instanceMirror.reflectMethod(method.asMethod).apply().asInstanceOf[Int]
        Equal(Polynomial.fromConstant(res)).definitelySubsetOf(constraint)
    }

  }

  private def getPropertyParams(symbol: SymbolApi): PropertyParams = symbol.typeSignature match {
    case TypeRef(_, IntSymbol, _) =>
      PropertyParams[Int](new ExpressionFactory[Int](TypeFacade), parameterConstraints(symbol))
    case TypeRef(_, DoubleSymbol, _) =>
      PropertyParams[Double](new ExpressionFactory[Double](TypeFacade), parameterConstraints(symbol))
  }

  private def methodConstraints(method: MethodSymbol) = {
    val globalSymbol = method.asInstanceOf[global.MethodSymbol]
    BoundsFactory.fromSymbol(globalSymbol)
  }

  private def parameterConstraints(param: SymbolApi) = {
    val globalSymbol = param.asInstanceOf[global.Symbol]
    BoundsFactory.fromSymbol(globalSymbol)
  }

  import org.scalacheck.Prop

  type P = PropertyParams

  private def property[R: RichNumeric: TypeTag]
    (p1: P)
    (method: MethodMirror, constraint: Constraint): Prop = {

    forAll(p1.generator) { n: Any =>
      val res = method.apply(n).asInstanceOf[R]
      Equal(Polynomial.fromConstant(res)).definitelySubsetOf(constraint)
    }(propBoolean, p1.shrink, prettyAny)
  }

  private def property[R: RichNumeric: TypeTag]
    (p1: P, p2: P)
    (method: MethodMirror, constraint: Constraint): Prop = {

    forAll { (n1: Any, n2: Any) =>
      val res = method.apply(n1, n2).asInstanceOf[R]
      Equal(Polynomial.fromConstant(res)).definitelySubsetOf(constraint)
    }(propBoolean, p1.arbitrary, p1.shrink, prettyAny, p2.arbitrary, p2.shrink, prettyAny)
  }

  private def property[R: RichNumeric: TypeTag]
  (p1: P, p2: P, p3: P)
  (method: MethodMirror, constraint: Constraint): Prop = {

    forAll { (n1: Any, n2: Any, n3: Any) =>
      val res = method.apply(n1, n2, n3).asInstanceOf[R]
      Equal(Polynomial.fromConstant(res)).definitelySubsetOf(constraint)
    }(propBoolean, p1.arbitrary, p1.shrink, prettyAny,
                   p2.arbitrary, p2.shrink, prettyAny,
        p3.arbitrary, p3.shrink, prettyAny)
  }

  private def property[R: RichNumeric: TypeTag]
  (p1: P, p2: P, p3: P, p4: P)
  (method: MethodMirror, constraint: Constraint): Prop = {

    forAll { (n1: Any, n2: Any, n3: Any, n4: Any) =>
      val res = method.apply(n1, n2, n3, n4).asInstanceOf[R]
      Equal(Polynomial.fromConstant(res)).definitelySubsetOf(constraint)
    }(propBoolean, p1.arbitrary, p1.shrink, prettyAny,
        p2.arbitrary, p2.shrink, prettyAny,
        p3.arbitrary, p3.shrink, prettyAny,
        p4.arbitrary, p4.shrink, prettyAny
    )
  }

  private def property[R: RichNumeric: TypeTag]
  (p1: P, p2: P, p3: P, p4: P, p5: P)
  (method: MethodMirror, constraint: Constraint): Prop = {

    forAll { (n1: Any, n2: Any, n3: Any, n4: Any, n5: Any) =>
      val res = method.apply(n1, n2, n3, n4, n5).asInstanceOf[R]
      Equal(Polynomial.fromConstant(res)).definitelySubsetOf(constraint)
    }(propBoolean, p1.arbitrary, p1.shrink, prettyAny,
        p2.arbitrary, p2.shrink, prettyAny,
        p3.arbitrary, p3.shrink, prettyAny,
        p4.arbitrary, p4.shrink, prettyAny,
        p5.arbitrary, p5.shrink, prettyAny
      )
  }
  private def property[R: RichNumeric: TypeTag]
  (p1: P, p2: P, p3: P, p4: P, p5: P, p6: P)
  (method: MethodMirror, constraint: Constraint): Prop = {

    forAll { (n1: Any, n2: Any, n3: Any, n4: Any, n5: Any, n6: Any) =>
      val res = method.apply(n1, n2, n3, n4, n5, n6).asInstanceOf[R]
      Equal(Polynomial.fromConstant(res)).definitelySubsetOf(constraint)
    }(propBoolean, p1.arbitrary, p1.shrink, prettyAny,
        p2.arbitrary, p2.shrink, prettyAny,
        p3.arbitrary, p3.shrink, prettyAny,
        p4.arbitrary, p4.shrink, prettyAny,
        p5.arbitrary, p5.shrink, prettyAny,
        p6.arbitrary, p6.shrink, prettyAny
      )
  }
  private def property[R: RichNumeric: TypeTag]
  (p1: P, p2: P, p3: P, p4: P, p5: P, p6: P, p7: P)
  (method: MethodMirror, constraint: Constraint): Prop = {

    forAll { (n1: Any, n2: Any, n3: Any, n4: Any, n5: Any, n6: Any, n7: Any) =>
      val res = method.apply(n1, n2, n3, n4, n5, n6, n7).asInstanceOf[R]
      Equal(Polynomial.fromConstant(res)).definitelySubsetOf(constraint)
    }(propBoolean, p1.arbitrary, p1.shrink, prettyAny,
        p2.arbitrary, p2.shrink, prettyAny,
        p3.arbitrary, p3.shrink, prettyAny,
        p4.arbitrary, p4.shrink, prettyAny,
        p5.arbitrary, p5.shrink, prettyAny,
        p6.arbitrary, p6.shrink, prettyAny,
        p7.arbitrary, p7.shrink, prettyAny
      )
  }
  private def property[R: RichNumeric: TypeTag]
  (p1: P, p2: P, p3: P, p4: P, p5: P, p6: P, p7: P, p8: P)
  (method: MethodMirror, constraint: Constraint): Prop = {

    forAll { (n1: Any, n2: Any, n3: Any, n4: Any, n5: Any, n6: Any, n7: Any, n8: Any) =>
      val res = method.apply(n1, n2, n3, n4, n5, n6, n7, n8).asInstanceOf[R]
      Equal(Polynomial.fromConstant(res)).definitelySubsetOf(constraint)
    }(propBoolean, p1.arbitrary, p1.shrink, prettyAny,
        p2.arbitrary, p2.shrink, prettyAny,
        p3.arbitrary, p3.shrink, prettyAny,
        p4.arbitrary, p4.shrink, prettyAny,
        p5.arbitrary, p5.shrink, prettyAny,
        p6.arbitrary, p6.shrink, prettyAny,
        p7.arbitrary, p7.shrink, prettyAny,
        p8.arbitrary, p8.shrink, prettyAny
      )
  }

  class PropertyParams(
    val arbitrary: Arbitrary[Any],
    val shrink: Shrink[Any],
    val constraints: Constraint,
    expressionFactory: ExpressionFactory[Any]
  ) {
    def generator = arbitrary.arbitrary suchThat
      (n => Equal(expressionFactory.fromConstant(n)) definitelySubsetOf constraints)
  }

  object PropertyParams {
    def apply[T](expressionFactory: ExpressionFactory[T],
                 constraint: Constraint = NoConstraints)
                (implicit arbitrary: Arbitrary[T], shrink: Shrink[T]) =
      new PropertyParams(arbitrary.asInstanceOf[Arbitrary[Any]],
        shrink.asInstanceOf[Shrink[Any]], constraint,
        expressionFactory.asInstanceOf[ExpressionFactory[Any]])
  }

  override def checkBounds(context: PropertyRunner.Context)(tree: global.Tree): PropertyRunner.BoundedType = ???

  override def reportError(error: PropertyRunner.BoundedTypeError): Unit = ???
}
