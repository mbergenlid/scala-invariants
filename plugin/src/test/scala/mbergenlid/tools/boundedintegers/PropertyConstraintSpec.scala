package mbergenlid.tools.boundedintegers

class PropertyConstraintSpec extends PluginTestRunner {

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

  test("Return property constrained object") {
    compile(
      """
        |@Property("length", LessThan(10))
        |val s = fiveCharacterString
        |
        |true
      """.stripMargin)(Nil)
  }

  test("Property constraint should apply to property alone too") {
    compile(
      """
        |@Property("length", LessThan(10))
        |val s = arrayOfLength5
        |
        |@LessThan(10)
        |val l = s.length
        |true
      """.stripMargin)(Nil)
  }

  test("Property constraints with transitive constraints") {
    compile(
      """
        |val source = new SafeArray(randomInteger)
        |val x = anotherRandomInteger
        |val y = intBetween5And10
        |
        |if(source.length < x) {
        |  @Property("length", LessThan(x))
        |  val sa1 = source
        |}
        |
        |if(source.length == y) {
        |  @Property("length", LessThan(11), GreaterThan(4))
        |  val sa2 = source
        |}
        |
        |true
      """.stripMargin)(Nil)
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

  ignore("Manual property constraint") {
    compile(
      """
        |class Container(_value: Int) {
        |  def value = _value
        |}
        |
        |val source = new Container(randomInteger)
        |
        |if(source.value < 10) {
        |  @Property("value", LessThan(10))
        |  val c1 = source
        |}
      """.stripMargin)(Nil)
  }

//  ignore("Manual non constant property constraint") {
//    compile(
//      """
//        |class Container() {
//        |  private[this] var _value: Int = _
//        |
//        |  def value = _value
//        |}
//        |
//        |val source = new Container()
//        |
//        |if(source.value < 10) {
//        |  @Property("value", LessThan(10))
//        |  val c1 = source
//        |}
//      """.stripMargin)(List(11))
//  }
}
