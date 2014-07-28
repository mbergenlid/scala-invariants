package mbergenlid.tools.test.testspecs

import org.scalatest.FunSuite
import mbergenlid.scalainvariants.test.TestParser
import scala.util.parsing.input.{StreamReader, CharSequenceReader}
import java.io.{File, FileReader}

class TestParserSpec extends FunSuite {

  test("Input with error lines") {
    val parser = new TestParser {}

    val input = new CharSequenceReader("Errors: 14, 17, 4711\n")

    val parseResult = parser.parseAll(parser.errorSpec, input)
    assert(parseResult.successful)

    val lines = parseResult.get
    println(lines)
    assert(lines === List(14, 17, 4711))
  }

  test("Parsing simple test specification") {
    val parser = new TestParser {}

    val input = new CharSequenceReader(
      """
        |/**
        | * Errors: 1, 5
        | */
        |package bla
        |
        |object SomeClass {
        |  /**
        |   * scaladoc
        |   */
        |  def scalaStuff: String = "ASD"
        |}
      """.stripMargin
      )

    val parseResult = parser.parse(parser.testSpecification, input)
    assert(parseResult.successful)

    val result = parseResult.get
    assert(result === parser.TestSpecification(List(1,5)))
  }

  test("Parsing siple test specification from file") {
    val parser = new TestParser {}

    val file = new File(this.getClass.getClassLoader.getResource("ParserInput.scala").toURI)
    val input = StreamReader(new FileReader(file))

    val parseResult = parser.parse(parser.testSpecification, input)
    assert(parseResult.successful)

    val result = parseResult.get
    assert(result === parser.TestSpecification(List(1,5)))

  }
}
