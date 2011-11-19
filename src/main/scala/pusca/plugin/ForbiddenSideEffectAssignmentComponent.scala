package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags

class ForbiddenSideEffectAssignmentComponent(val global: Global) extends PluginComponent with PuscaDefinitions {
  import global._

  val runsAfter = List("typer")
  val phaseName = "forbiddenSideEffectAssignment"
  def newPhase(prev: Phase) = new ForbiddenSideEffectAssignment(prev)

  class ForbiddenSideEffectAssignment(prev: Phase) extends StdPhase(prev) {

    override def apply(unit: CompilationUnit) {
      def handle(t: Tree): Unit = {
        t match {
          
          case v: ValDef if hasSideEffect(v.tpt.tpe) && !v.symbol.isSynthetic ⇒
            reporter.error(v.pos, "declaration of " + (if (v.symbol.isVariable) "var" else "val") +" with @sideEffect is not allowed")
          case _ ⇒ ()
        }
        t.children.foreach(handle)
      }
      handle(unit.body)
    }
  }
}