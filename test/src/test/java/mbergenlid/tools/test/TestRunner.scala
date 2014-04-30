package mbergenlid.tools.test

import org.scalatest.FunSuite
import mbergenlid.tools.test.utils.PropertyRunner


class TestRunner extends FunSuite {

  val Scalac = "/opt/scala/scala-2.10.3/bin/scalac"
  val Plugin = "../tmp/plugin.jar"

  test("First test") {
    val result = compile("src/main/resources/test/Test2.scala")

    assert(result == 0, "Expected source to compile")
    PropertyRunner.execute("test.Test2")
  }

  def compile(file: String) = {
    import scala.sys.process._

    Seq(Scalac,
      "-cp",
      System.getProperty("java.class.path"),
      s"-Xplugin:$Plugin",
      "-d", "/home/marcus/work/plugin/test/target/scala-2.10/classes/", file).!
  }
}
