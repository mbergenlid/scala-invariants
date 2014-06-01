package mbergenlid.tools.test

import org.scalatest.FunSuite
import mbergenlid.tools.test.utils.PropertyRunner
import java.io.{FilenameFilter, File}


class TestRunner extends FunSuite {

  val Scalac = "/opt/scala/scala-2.10.3/bin/scalac"
  val Plugin = findPluginJar()

  registerTests()

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

  private def findPluginJar(): String = {
    val pluginDir = new File(System.getProperty("PLUGIN_ROOT", "."))
    val target = new File(pluginDir, "target/scala-2.10")
    val jars = target.listFiles(new FilenameFilter() {
      def accept(file: File, name: String) =
        name.endsWith(".jar") && !(name.endsWith("-javadoc.jar") || name.endsWith("-sources.jar"))
    })
    assert(jars.length == 1)
    jars(0).getAbsolutePath
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
