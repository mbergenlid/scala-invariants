package mbergenlid.tools.boundedintegers

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import validators._


class BoundedIntegersPlugin(val global: Global) extends Plugin {

  val name = "bounded-integers"
  val description = "allows variable interpolation in strings"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    import global._
    import global.definitions._

    val global = BoundedIntegersPlugin.this.global
    override val runsAfter = List("typer")

    val phaseName = "bounded-integers"
    def newPhase(_prev: Phase) = new BoundedIntegersPhase(_prev)

    class BoundedIntegersPhase(prev: Phase) extends StdPhase(prev) {
      override def name = BoundedIntegersPlugin.this.name
      val typeChecker = new BoundedTypeChecker(global)  with MethodApplication
                                                        with IfExpression
                                                        with Assignment
                                                        with MethodDefinition

      def apply(unit: CompilationUnit) {
        val errors = typeChecker.checkBoundedTypes(
          unit.body.asInstanceOf[typeChecker.global.Tree])
        errors foreach { e =>
          unit.error(e.pos.asInstanceOf[Position], e.message)
        }
      }
    }
  }
}
