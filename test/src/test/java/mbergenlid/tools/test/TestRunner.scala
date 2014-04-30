package mbergenlid.tools.test

import org.scalatest.FunSuite
import mbergenlid.tools.test.utils.PropertyRunner
import java.io.File


class TestRunner extends FunSuite {

  val Scalac = "/opt/scala/scala-2.10.3/bin/scalac"
  val Plugin = "../tmp/plugin.jar"

  registerTests()
//  test("First test") {
//    val result = compile("src/main/resources/test/Test2.scala")
//
//    assert(result == 0, "Expected source to compile")
//    PropertyRunner.execute("test.Test2")
//  }

  private def registerTests() {
    val testDir = this.getClass.getClassLoader.getResource("test")

    val file = new File(testDir.toURI)

    file.listFiles().filter(_.getName.endsWith(".scala")).foreach { f =>
      test(f.getName) {
        val result = compile(f.getAbsolutePath)

        assert(result == 0, "Expected source to compile")

        val name = f.getName
        PropertyRunner.execute(s"test.${name.dropRight(6)}")
      }
    }
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
