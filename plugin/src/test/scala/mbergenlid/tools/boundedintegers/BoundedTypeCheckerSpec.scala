package mbergenlid.tools.boundedintegers


class BoundedTypeCheckerSpec extends PluginTestRunner
  with MyUniverse {
  
  test("Failure with constant argument") {
    compile("""
    |testMethod(4)
    |testMethod(11)
    |testMethod(-1)
    """.stripMargin)(List(3,4))
  }

  test("Should fail if called with variable that is not itself within range") {
    compile("""
          |val x = 11
          |testMethod(x)
          """.stripMargin)(List(3))
  }

  test("Should not fail if variable is annotated") {
    compile("""
              |@Equal(10) val x = 10
              |testMethod(x)
              |1
            """.stripMargin)(Nil)
  }

  test("Should fail if called with arbitrary Int expression") {
    compile("""
          |testMethod(anotherRandomInteger)
          |val x = anotherRandomInteger
          |testMethod(x)
          """.stripMargin)(List(2, 4))
  }

  test("Test return statement") {
    compile("""
              |@GreaterThanOrEqual(0)
              |@LessThanOrEqual(10)
              |val x = 11
              |1
            """.stripMargin)(List(4))
  }


  test("Validation in middle of boolean expression") {
    compile("""
              |val x = randomInteger
              |
              |if(x < 10 && testMethod(x)) println("Should compile")
              |if(x > 0 && testMethod(x)) println("Should not compile")
            """.stripMargin)(List(5))
  }

  test("Implicitly bound to method") {
    compile(
      """
        |val x = 5
        |@Equal(x)
        |val y = x
        |
        |testMethod(y)
      """.stripMargin)(Nil)
  }

  test("Bound constant to symbol") {
    compile(
      """
        |val x = anotherRandomInteger
        |def upperBound(@LessThan(x)a: Int) = true
        |def upperBoundInclusive(@LessThanOrEqual(x)a: Int) = true
        |def lowerBound(@GreaterThan(x)a: Int) = true
        |def lowerBoundInclusive(@GreaterThanOrEqual(x)a: Int) = true
        |def equal(@Equal(x)a: Int) = true
        |
        |if(x > 3) upperBound(2)
        |if(x < 3) upperBound(2) //Fail
        |if(x > 1) upperBound(2) //Fail
        |if(x > 2) upperBound(2)
        |if(x < 2) upperBound(2) //Fail
        |
        |if(x < 1) lowerBound(2)
        |if(x > 1) lowerBound(2) //Fail
        |if(x < 3) lowerBound(2) //Fail
        |if(x < 2) lowerBound(2)
        |if(x > 2) lowerBound(2) //Fail
        |
        |if(x > 3) upperBoundInclusive(2)
        |if(x < 3) upperBoundInclusive(2) //Fail
        |if(x > 1) upperBoundInclusive(2)
        |if(x > 2) upperBoundInclusive(2)
        |if(x < 2) upperBoundInclusive(2) //Fail
        |
        |if(x < 1) lowerBoundInclusive(2)
        |if(x > 1) lowerBoundInclusive(2) //Fail
        |if(x < 3) lowerBoundInclusive(2)
        |if(x < 2) lowerBoundInclusive(2)
        |if(x > 2) lowerBoundInclusive(2) //Fail
        |true
      """.stripMargin)(List(10, 11, 13, 16, 17, 19, 22, 25, 28, 31))
  }

  test("Bound constant to symbol with inclusive constraints") {
    compile(
      """
        |val x = anotherRandomInteger
        |def upperBound(@LessThan(x)a: Int) = true
        |def upperBoundInclusive(@LessThanOrEqual(x)a: Int) = true
        |def lowerBound(@GreaterThan(x)a: Int) = true
        |def lowerBoundInclusive(@GreaterThanOrEqual(x)a: Int) = true
        |def equal(@Equal(x)a: Int) = true
        |
        |if(x >= 3) upperBound(2)
        |if(x <= 3) upperBound(2) //Fail
        |if(x >= 1) upperBound(2) //Fail
        |if(x >= 2) upperBound(2)
        |if(x <= 2) upperBound(2) //Fail
        |
        |if(x <= 1) lowerBound(2)
        |if(x >= 1) lowerBound(2) //Fail
        |if(x <= 3) lowerBound(2) //Fail
        |if(x <= 2) lowerBound(2)
        |if(x >= 2) lowerBound(2) //Fail
        |
        |if(x >= 3) upperBoundInclusive(2)
        |if(x <= 3) upperBoundInclusive(2) //Fail
        |if(x >= 1) upperBoundInclusive(2) //Fail
        |if(x >= 2) upperBoundInclusive(2)
        |if(x <= 2) upperBoundInclusive(2) //Fail
        |
        |if(x <= 1) lowerBoundInclusive(2)
        |if(x >= 1) lowerBoundInclusive(2) //Fail
        |if(x <= 3) lowerBoundInclusive(2) //Fail
        |if(x <= 2) lowerBoundInclusive(2)
        |if(x >= 2) lowerBoundInclusive(2) //Fail
        |
        |true
      """.stripMargin)(List(10, 11, 13, 16, 17, 19, 22, 23, 25, 28, 29, 31))
  }

  test("Safe array test") {
    compile(
      """
        |new SafeArray(-2) //Not ok
        |val sa1 = new SafeArray(10) //Ok
        |if(sa1.length > 10)
        |  sa1(4)
        |
        |val sa2 = new SafeArray(10) //Ok
        |if(sa1.length > 10)
        |  sa2(4) //not Ok
        |true
      """.stripMargin)(List(2, 9))
  }

  test("Property constraints") {
    compile(
      """
        |val source = new SafeArray(randomInteger)
        |
        |if(source.length > 10) {
        |  @Property("length", GreaterThan(5))
        |  val sa1 = source
        |}
        |
        |@Property("length", GreaterThan(5))
        |val sa2 = source
        |
        |if(source.length > 5 && source.length < 10) {
        |  @Property("length", GreaterThan(5), LessThan(10))
        |  val sa3 = source
        |}
        |true
      """.stripMargin)(List(10))
  }

  test("Property constraints2") {
    compile(
      """
        |class ArrayContainer(val sa: SafeArray)
        |val source1 = new ArrayContainer(new SafeArray(randomInteger))
        |val source2 = new ArrayContainer(new SafeArray(randomInteger))
        |
        |if(source1.sa.length > 10) {
        |  @Property("length", GreaterThan(5))
        |  val sa1 = source1.sa
        |}
        |
        |@Property("length", GreaterThan(5))
        |val sa2 = source1.sa
        |
        |if(source1.sa.length > 10) {
        |  @Property("length", GreaterThan(5))
        |  val sa3 = source2.sa
        |}
        |
        |if(source1.sa.length > 5 && source1.sa.length < 10) {
        |  @Property("length", GreaterThan(5), LessThan(10))
        |  val sa4 = source1.sa
        |}
        |
        |if(source1.sa.length > 5 && source1.sa.length < 10) {
        |  @Property("length", GreaterThan(5), LessThan(10))
        |  val sa5 = source2.sa
        |}
        |true
      """.stripMargin)(List(12, 16, 26))
  }

  test("Property constraints on Strings") {
    compile(
      """
        |object Test {
        |  val source = "asdasd"
        |}
        |
        |import Test._
        |if(source.length > 10) {
        |  @Property("length", GreaterThan(5))
        |  val sa1 = source
        |}
        |
        |@Property("length", GreaterThan(5))
        |val sa2 = source
        |
        |if(source.length > 5 && source.length < 10) {
        |  @Property("length", GreaterThan(5), LessThan(10))
        |  val sa3 = source
        |}
        |true
      """.stripMargin)(List(13))
  }

  test("Invalid property constraints") {
    compile(
      """
        |val source = new SafeArray(randomInteger)
        |
        |@Property("nonExistingProperty", GreaterThan(1))
        |val sa = source
        |
        |@Property("length")
        |val sa1 = source
        |
        |false
      """.stripMargin)(List(4, 7))
  }

  test("Symbol chains") {
    compile(
      """
        |object T1 {
        |  @Equal(1)
        |  val x = 1
        |}
        |object T2 {
        |  val x = 2
        |}
        |
        |@Equal(11)
        |val y = T1.x + 10
        |@Equal(1)
        |val z = T2.x
        |true
      """.stripMargin)(List(13))
  }
}
