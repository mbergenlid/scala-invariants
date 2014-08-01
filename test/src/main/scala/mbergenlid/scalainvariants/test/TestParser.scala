package mbergenlid.scalainvariants.test

import java.io.{BufferedReader, FileInputStream, InputStreamReader, File}

import mbergenlid.scalainvariants.test.TestParser.{ErrorDef, TestSpecification}

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.JavaTokenParsers


/**
 * Parser for the meta data of a test scala file that should
 * be run through the compiler.
 *
 * The Meta data contains information about the expected result
 * of the compilation like what lines are expected to produce an error.
 *
 */
object TestParser {
  type ErrorList = List[ErrorDef]
  case class ErrorDef(line: Int)
  case class TestSpecification(errors: ErrorList)

  def parse(input: File): TestSpecification = {
    val errors =
      forEachLine(input) {
        case (line, index) if line.endsWith("//error") => ErrorDef(index)
      }
    TestSpecification(errors)
  }

  def forEachLine[T](file: File)(f: PartialFunction[(String, Int), T]): List[T] = {
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))

    val buffer = new ArrayBuffer[T]
    var lineNumber = 1
    var line = reader.readLine()
    while(line != null) {
      if(f.isDefinedAt(line, lineNumber))
        buffer += f(line, lineNumber)

      line = reader.readLine()
      lineNumber += 1
    }
    reader.close()

    buffer.toList
  }
}
