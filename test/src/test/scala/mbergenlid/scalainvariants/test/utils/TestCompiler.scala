package mbergenlid.scalainvariants.test.utils

import org.scalatest.{Tag, FunSuite}
import scala.sys.process.ProcessLogger

trait TestCompiler {
  self: FunSuite =>


  val ScalaHome = getEnv("SCALA_HOME")
  val Scalac = s"$ScalaHome/bin/scalac"
  val Plugin = getProperty("PLUGIN")
  val TestOutput = getProperty("TEST_OUTPUT")


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

    val builder = Process(commandLine, None)
//      "JAVA_OPTS" -> "-agentlib:jdwp=transport=dt_socket,server=n,address=marcus-Aspire-V5-571G:5005,suspend=y")
    if(logger.isDefined) builder.!(logger.get)
    else builder.!
  }
}
