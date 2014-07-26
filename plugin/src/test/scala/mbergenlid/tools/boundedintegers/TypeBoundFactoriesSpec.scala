package mbergenlid.tools.boundedintegers


class TypeBoundFactoriesSpec extends PluginTestRunner {

  test("Numeric method application") {
    compile(
      """
        |@Equal("array.length")
        |def testMethod(array: Array[Int]): Int = array.length
        |
        |val sa = Array(1,2,3,4,5)
        |
        |@Equal(sa.length)
        |val index = testMethod(sa)
        |
        |true
      """.stripMargin)(Nil)

  }

  test("PropertyType with no direct constraints") {
    val bounds = expression(
      """
        |class TestClass {
        |  val prop: Int = 3
        |}
        |
        |val tc = new TestClass
        |tc
      """.stripMargin)

    val props = bounds.constraint match {
      case a: cut.And => a.constraints.collect {
        case p@cut.PropertyConstraint(symbol, c) if symbol.name.toString == "prop" => p
      }
      case p@cut.PropertyConstraint(symbol, c) if symbol.name.toString == "prop" => List(p)
      case _ => Nil
    }

    assert(props.size === 1)
    val prop = props.head

    assert(prop.symbol.name.toString === "prop")
    assert(prop.constraint.exists(ec => ec.isInstanceOf[cut.Equal]))
  }

  test("Array with no direct constraints") {
    val bounds = expression(
      """
        |val a = Array(1,2,3,4,5)
        |a
      """.stripMargin)

    assert(bounds.constraint.isInstanceOf[cut.And])
    val props = bounds.constraint.asInstanceOf[cut.And].constraints.collect {
      case p @ cut.PropertyConstraint(symbol, c) if symbol.name.toString == "length" => p
    }
    assert(props.size === 1)
    val prop = props.head
    assert(prop.symbol.name.toString === "length")
    assert(prop.constraint.exists(ec => ec.isInstanceOf[cut.Equal]))
  }
}