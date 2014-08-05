package mbergenlid.scalainvariants.test

import org.scalatest.FunSuite
import mbergenlid.scalainvariants.test.utils.{TestScanner, TestCompiler}
import java.net.URL
import java.io.File
import scala.sys.process.ProcessLogger

class CompilerTestRunner extends FunSuite with TestCompiler with TestScanner {

  override protected def evaluate(file: File): Unit = {
    val specification = TestParser.parse(file)
    val logger = new Logger(file.getName)
    compile(file.getAbsolutePath, logger)

    val actualErrors = logger.errors
    for(error <- actualErrors) {
      if(!specification.errors.exists(_.line == error.line))
        fail(s"Unexpected compiler error:\n${error.message}")
    }
    for(expected <- specification.errors) {
      if(!actualErrors.exists(_.line == expected.line))
        fail(s"Expected compiler error on line $expected")
    }
  }

  class Logger(fileName: String) extends ProcessLogger {
    val Pattern = s""".*$fileName:(\\d+): error:.*""".r
    private var _lines: List[CompileError] = Nil
    private var currentLine: Int = -1
    private val messageBuilder = new StringBuilder

    def errors: List[CompileError] = _lines.reverse

    override def buffer[T](f: => T): T = {
      val res = f
      appendError()
      res
    }

    private def appendError(): Unit = {
      if(currentLine != -1) {
        _lines = CompileError(currentLine, messageBuilder.toString()) :: _lines
        messageBuilder.clear()
      }
    }

    override def err(s: => String): Unit = s match {
      case Pattern(line) =>
        appendError()
        currentLine = line.toInt
        messageBuilder.append(s).append("\n")
      case _ =>
        messageBuilder.append(s).append("\n")
    }

    override def out(s: => String): Unit = {
      println(s)
    }
  }

  case class CompileError(line: Int, message: String)

  override protected def searchPath: URL =
    this.getClass.getClassLoader.getResource("compiletests")
}
