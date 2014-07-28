package mbergenlid.scalainvariants.test

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}
import scala.util.parsing.input.Reader


/**
 * Parser for the meta data of a test scala file that should
 * be run through the compiler.
 *
 * The Meta data contains information about the expected result
 * of the compilation like what lines are expected to produce an error.
 *
 */
trait TestParser extends RegexParsers {

  def parse(input: Reader[Char]): ParseResult[TestSpecification] =
    parse(testSpecification, input)

  def testSpecification: Parser[TestSpecification] =
    "/**" ~> (spec ^^ TestSpecification) <~ "*/"

  def spec: Parser[ErrorList] =
    "*".? ~> errorSpec

  def errorSpec: Parser[ErrorList] =
    "Errors: " ~> errorList

  def errorList: Parser[ErrorList] =
    repsep(lineNumber, ",")

  def lineNumber: Parser[Int] =
    """\d+""".r ^^ {Integer.parseInt}


  type ErrorList = List[Int]
  case class TestSpecification(errors: ErrorList)

}
