package mbergenlid.scalainvariants.test.utils

import java.io.File
import java.net.URL

import org.scalatest.{Tag, FunSuite}
import scala.sys.process.ProcessLogger

trait TestCompiler {
  self: FunSuite =>


//  val ScalaHome = getEnv("SCALA_HOME")
  val Scalac = "scalac"
  val Plugin = getProperty("PLUGIN")
  val TestOutput = getProperty("TEST_OUTPUT")

  protected def searchPath: URL
  protected def evaluate(file: File): Unit

  registerTests()

  private def registerTests() {
    val testDir: URL = searchPath

    val file = new File(testDir.toURI)
    file.listFiles().
        filter(_.getName.endsWith(".scala")).foreach { f =>
      test(f.getName, Tag(f.getName.dropRight(".scala".length))) {
        evaluate(f)
      }
    }
  }

  private def getProperty(name: String) = {
    val prop = System.getProperty(name)
    if(prop != null) prop
    else throw new IllegalArgumentException(s"System property $name must be set")
  }

  private def getEnv(name: String) = {
    val env = System.getenv(name)
    if(env != null) env
    else throw new IllegalArgumentException(s"Environment variable $name must be set")
  }

  def compile(file: String): Int =
    compile(file, None)

  def compile(file: String, logger: ProcessLogger): Int =
    compile(file, Some(logger))

  private def compile(file: String, logger: Option[ProcessLogger]) = {
    import scala.sys.process._

    val commandLine =
      Seq(Scalac,
        "-cp",
        Plugin,
        s"-Xplugin:$Plugin",
        "-d", TestOutput, file)

    if(logger.isDefined) commandLine.!(logger.get)
    else commandLine.!
  }
}
