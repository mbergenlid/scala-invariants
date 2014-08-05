package mbergenlid.scalainvariants.test.utils

import java.io.File
import java.net.URL

import org.scalatest.{Tag, FunSuite}

trait TestScanner {
  self: FunSuite =>

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
}
