package mbergenlid.tools.test

import org.scalatest.FunSuite
import mbergenlid.tools.test.utils.TestCompiler
import java.net.URL
import java.io.{FileReader, File}
import mbergenlid.scalainvariants.test.TestParser
import scala.util.parsing.input.StreamReader
import scala.sys.process.ProcessLogger

class CompilerTestRunner extends FunSuite with TestCompiler with TestParser {

  override protected def evaluate(file: File): Unit = {
    val input = StreamReader(new FileReader(file))
    val result = parse(input)
    if(result.successful) {
      val specification = result.get
      val logger = new Logger(file.getName)
      compile(file.getAbsolutePath, logger)

      assert(logger.lines === specification.errors)
    } else {
      fail(s"Failed to parse test specification in file $file")
    }
  }

  class Logger(fileName: String) extends ProcessLogger {

    val Pattern = s""".*$fileName:(\\d+): error:.*""".r
    private var _lines: List[Int] = Nil

    def lines = _lines.reverse

    override def buffer[T](f: => T): T = f

    override def err(s: => String): Unit = s match {
      case Pattern(line) =>
        _lines = line.toInt :: _lines
        println(s)
      case _ =>
        println(s)
    }

    override def out(s: => String): Unit = println(s)
  }

  override protected def searchPath: URL =
    this.getClass.getClassLoader.getResource("compiletests")
}
