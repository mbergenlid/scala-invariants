package mbergenlid.tools.test

import org.scalatest.FunSuite

class TestRunner extends FunSuite {

  val Scalac = "/opt/scala/scala-2.10.3/bin/scalac"
  val Plugin = "../tmp/plugin.jar"

  test("First test") {
    println(System.getenv("SCALA_HOME"))
    println(System.getProperty("java.class.path"))
    compile("src/main/scala/mbergenlid/tools/test/Test1.scala")
  }

  def compile(file: String) {
    import scala.sys.process._

    val cwd = Seq(Scalac,
      "-cp",
      System.getProperty("java.class.path"),
      s"-Xplugin:$Plugin", file).!
    println(cwd)
  }
}
