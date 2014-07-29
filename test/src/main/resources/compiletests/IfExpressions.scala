/**
 * Errors: 21, 27
 */
package compiletests

import mbergenlid.tools.boundedintegers.annotations._
import scala.util.Random

object IfExpressions {

  val anotherRandomInteger = 20

  def testMethod(@GreaterThanOrEqual(0)
                 @LessThanOrEqual(10) a: Int) = a == 3

  def test1() = {
    val x = anotherRandomInteger
    if(x < 11 && -1 < x)
      testMethod(x)
    else
      testMethod(x)
  }

  def successfulElse() = {
    val x = anotherRandomInteger
    if(x > 10 || x < 0)
      testMethod(x)
    else
      testMethod(x)
  }
}