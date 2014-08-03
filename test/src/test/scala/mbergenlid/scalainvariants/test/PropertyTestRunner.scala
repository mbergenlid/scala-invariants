package mbergenlid.scalainvariants.test

import java.net.URL

import org.scalatest.{Outcome, FunSuite}
import mbergenlid.scalainvariants.test.utils.{TestCompiler, PropertyRunner}
import java.io.File


class PropertyTestRunner extends FunSuite with TestCompiler {

  override protected def searchPath: URL =
    this.getClass.getClassLoader.getResource("propertytests")

  override protected def evaluate(file: File): Unit = {
    val result = compile(file.getAbsolutePath)
    assert(result == 0, "Expected source to compile")
    PropertyRunner.execute(s"propertytests.${file.getName.dropRight(6)}")
  }


}
