package mbergenlid.tools.test.testspecs

import mbergenlid.scalainvariants.test.TestParser.{ErrorDef, TestSpecification}
import org.scalatest.FunSuite
import mbergenlid.scalainvariants.test.TestParser
import java.io.File

class TestParserSpec extends FunSuite {

  test("Parsing simple test specification from file") {

    val file = new File(this.getClass.getClassLoader.getResource("ParserInput.scala").toURI)

    val result = TestParser.parse(file)

    assert(result === TestSpecification(List(ErrorDef(7),ErrorDef(10))))

  }
}
