package mbergenlid.tools.boundedintegers


class TypeBoundFactoriesSpec extends PluginTestRunner {

  /**
   *
   * int -> checkBounds(5) = NumericType(Equal(5))
   * array -> checkBounds(sa) = PropertyType(PropertyConstraint(length, sa.length)
   *                                property(length) = ==sa.length
   * PropertyConstraint(length, ==sa.length) flatMap
   *  PropertyConstraint(length, ==sa.length)
   *
   * args = (
   *   array -> PropertyBounds(sa, ...)
   * )
   *
   * ec = Equal("array.length")
   *
   * sa.length
   *
   * params = array.length
   *
   * symbol = array.length
   * args.find(t => symbol.matchesPrefix(t._1)) match {
   *    case Some((sym, bounds)) => bounds.property()
   * }
   *
   * ec.extractSymbols.collect {
   *   case s if args.
   * }
   */
  test("Numeric method application") {
    val bounds = expression(
      """
        |@Equal("array.length")
        |def testMethod(array: Array[Int]): Int = array.length
        |
        |val sa = Array(1,2,3,4,5)
        |testMethod(sa)
      """.stripMargin)

    println(bounds.constraint.prettyPrint())
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