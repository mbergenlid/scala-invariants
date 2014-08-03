package compiletests

import mbergenlid.tools.boundedintegers.annotations._

import scala.util.Random

object PropertyConstraints {

  def randomInteger = new Random().nextInt()

  @LessThanOrEqual(10)
  @GreaterThanOrEqual(5)
  def intBetween5And10 = 6

  def anotherRandomInteger = 20

  def propertyConstraintsOnArrays() = {
    object Test {
      val source = Array(1,2,3,4,5)
    }

    import Test._
    if(source.length > 10) {
      @Property("length", GreaterThan(5))
      val sa1 = source
    }

    @Property("length", GreaterThan(5))
    val sa2 = source //Fail => source is unbounded //error

    if(source.length > 5 && source.length < 10) {
      @Property("length", GreaterThan(5), LessThan(10))
      val sa3 = source
    }
    true
  }

  def propertyConstraints() = {
    val source = new Array(randomInteger)

    if(source.length > 10) {
      @Property("length", GreaterThan(5))
      val sa1 = source
    }

    @Property("length", GreaterThan(5))
    val sa2 = source //error

    if(source.length > 5 && source.length < 10) {
      @Property("length", GreaterThan(5), LessThan(10))
      val sa3 = source
    }
    true
  }

  def propertyConstraints2() = {
    class ArrayContainer(val sa: Array[Int])
    val source1 = new ArrayContainer(new Array(randomInteger))
    val source2 = new ArrayContainer(new Array(randomInteger))

    if(source1.sa.length > 10) {
      @Property("length", GreaterThan(5))
      val sa1 = source1.sa
    }

    @Property("length", GreaterThan(5))
    val sa2 = source1.sa  //Fail, source1.sa is unbounded //error

    if(source1.sa.length > 10) {
      @Property("length", GreaterThan(5))
      val sa3 = source2.sa //error
    }

    if(source1.sa.length > 5 && source1.sa.length < 10) {
      @Property("length", GreaterThan(5), LessThan(10))
      val sa4 = source1.sa
    }

    if(source1.sa.length > 5 && source1.sa.length < 10) {
      @Property("length", GreaterThan(5), LessThan(10))
      val sa5 = source2.sa //error
    }
    true
  }

  def propertyConstraintsOnStrings() = {
    object Test {
      val source = "asdasd"
    }

    import Test._
    if(source.length > 10) {
      @Property("length", GreaterThan(5))
      val sa1 = source
    }

    @Property("length", GreaterThan(5))
    val sa2 = source //Fail => source is unbounded //error

    if(source.length > 5 && source.length < 10) {
      @Property("length", GreaterThan(5), LessThan(10))
      val sa3 = source
    }
    true
  }

  def propertyConstraintsWithTransitiveConstraints() = {
    val source = new Array(randomInteger)
    val x = anotherRandomInteger
    val y = intBetween5And10

    if(source.length < x) {
      @Property("length", LessThan(x))
      val sa1 = source
    }

    if(source.length == y) {
      @Property("length", LessThan(11), GreaterThan(4))
      val sa2 = source
    }

    true
  }

  def invalidPropertyConstraints() = {
    val source = new Array[Int](randomInteger)

    @Property("nonExistingProperty", GreaterThan(1)) //error
    val sa = source

    @Property("length") //error
    val sa1 = source

    false
  }

  def automaticPropertyConstraints() = {
    class TestClass {
      val prop: Int = 3
    }

    val tc = new TestClass
    def method(@Property("prop", Equal(tc.prop)) bla: TestClass) = 4
    method(tc)

    val sa = Array(1,2,3,4,5)
    def arrayMethod(@Property("length", Equal(sa.length)) array: Array[Int]) = 5
    arrayMethod(sa)
  }

  def numericMethodApplication() = {
    @Equal("array.length")
    def testMethod(array: Array[Int]): Int = array.length

    val sa = Array(1,2,3,4,5)

    @Equal(sa.length)
    val index = testMethod(sa)

    index
  }
}