package mbergenlid.tools.test.utils

import java.io.{FilenameFilter, File}
import java.net.URL

import org.scalatest.FunSuite

trait TestCompiler {
  self: FunSuite =>

  val ScalaHome = System.getProperty("SCALA_HOME", "/opt/scala/scala-2.10.4")
  val Scalac = s"$ScalaHome/bin/scalac"
  val PluginRoot: String = System.getProperty("PLUGIN_ROOT", ".")
  val Plugin = findPluginJar()

  protected def searchPath: URL
  protected def evaluate(file: File): Unit

  registerTests()

  private def registerTests() {
    val testDir: URL = searchPath

    val file = new File(testDir.toURI)

    file.listFiles().filter(_.getName.endsWith(".scala")).foreach { f =>
      test(f.getName) {
        val result = compile(f.getAbsolutePath)

        assert(result == 0, "Expected source to compile")

        evaluate(f)
      }
    }
  }

  private def findPluginJar(): String = {

    val pluginDir = new File(PluginRoot)
    val target = new File(pluginDir, "target/scala-2.10")
    val jars = target.listFiles(new FilenameFilter() {
      def accept(file: File, name: String) =
        name.endsWith(".jar") && !(name.endsWith("-javadoc.jar") || name.endsWith("-sources.jar"))
    })
    assert(jars.length == 1)
    jars(0).getAbsolutePath
  }

  private def findAnnotationsJar(): String = {
    val annotationsDir = new File(PluginRoot, "annotations")
    val target = new File(annotationsDir, "target/scala-2.10")
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
      Plugin,
      s"-Xplugin:$Plugin",
      "-d", s"$PluginRoot/test/target/scala-2.10/classes/", file).!
  }
}
