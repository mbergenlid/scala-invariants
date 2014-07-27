package mbergenlid.tools.test

import java.net.URL

import org.scalatest.FunSuite
import mbergenlid.tools.test.utils.{TestCompiler, PropertyRunner}
import java.io.File


class PropertyTestRunner extends FunSuite with TestCompiler {

  override protected def searchPath: URL =
    this.getClass.getClassLoader.getResource("propertytests")

  override protected def evaluate(file: File): Unit =
    PropertyRunner.execute(s"propertytests.${file.getName.dropRight(6)}")
}
