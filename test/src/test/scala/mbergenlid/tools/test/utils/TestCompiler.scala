package mbergenlid.tools.test.utils

import java.io.{FilenameFilter, File}
import java.net.URL

import org.scalatest.FunSuite

trait TestCompiler {
  self: FunSuite =>


  val ScalaHome = getProperty("SCALA_HOME")
  val Scalac = s"$ScalaHome/bin/scalac"
  val Plugin = getProperty("PLUGIN")
  val TestOutput = getProperty("TEST_OUTPUT")

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

  private def getProperty(name: String) = {
    val prop = System.getProperty(name)
    if(prop != null) prop
    else throw new IllegalArgumentException(s"System property $name must be set")
  }

  def compile(file: String) = {
    import scala.sys.process._

//    println(s"$Scalac -cp $Plugin -Xplugin:$Plugin -d $TestOutput $file")
    Seq(Scalac,
      "-cp",
      Plugin,
      s"-Xplugin:$Plugin",
      "-d", TestOutput, file).!
  }
}
