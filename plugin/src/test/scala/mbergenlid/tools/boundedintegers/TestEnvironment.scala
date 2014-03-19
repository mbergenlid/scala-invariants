package mbergenlid.tools.boundedintegers

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.runtimeMirror
object TestEnvironment {
  lazy val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
}
