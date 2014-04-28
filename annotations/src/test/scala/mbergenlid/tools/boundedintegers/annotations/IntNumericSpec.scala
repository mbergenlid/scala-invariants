package mbergenlid.tools.boundedintegers.annotations

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop._
import mbergenlid.tools.boundedintegers.annotations.RichNumeric.{DoubleIsRichNumeric, LongIsRichNumeric, IntIsRichNumeric}
import org.scalacheck.util.Pretty

object IntNumericSpec extends Properties("IntIsRichNumeric") {

  def testProperty[T: RichNumeric: Arbitrary](toBigInt: T => BigDecimal)
                                             (op1: (BigDecimal, BigDecimal) => BigDecimal, op2: (T,T) => T) =
    forAll { (a: T, b: T) =>
      val result = op1(toBigInt(a), toBigInt(b))
      val num = implicitly[RichNumeric[T]]
      if(result > toBigInt(num.maxValue)) throws(classOf[ArithmeticException])(op2(a,b)) :|
        s"$a and $b = $result > ${num.maxValue}"
      else if(result < toBigInt(num.minValue)) throws(classOf[ArithmeticException])(op2(a,b)) :|
        s"$a and $b = $result < ${num.maxValue}"
      else (toBigInt(op2(a,b)) == result) :|
        s"$a and $b = ${toBigInt(op2(a,b))}: expected $result ::: Diff = ${toBigInt(op2(a,b)) - result}"
    }

  property("IntegerAddition") = testProperty[Int](BigDecimal.apply)(_+_, IntIsRichNumeric.tryPlus)
  property("LongAddition") =  testProperty[Long](BigDecimal.apply)(_+_, LongIsRichNumeric.tryPlus)
  property("IntSubtraction") =  testProperty[Int](BigDecimal.apply)(_-_, IntIsRichNumeric.tryMinus)
  property("LongAddition") = testProperty[Long](BigDecimal.apply)(_-_, LongIsRichNumeric.tryMinus)
  property("IntMultiplication") = testProperty[Int](BigDecimal.apply)(_*_, IntIsRichNumeric.tryTimes)
  property("LongMultiplication") = testProperty[Long](BigDecimal.apply)(_*_, LongIsRichNumeric.tryTimes)

//  property("DoubleAddition") = testProperty[Double](BigDecimal.apply)(_+_, DoubleIsRichNumeric.tryPlus)

}
