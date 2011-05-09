package pusca.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class PurePlugin(val global: Global) extends Plugin {
  val name = "pure"
  val description = "Enforces pureness"
  val components = List[PluginComponent]( //
    new AnnotatePureComponent(global), //
//        ShowTreeComponent, //
    new CheckPurityComponent(global),
    new WarnLostAssignments(global))

  import global._
  private object ShowTreeComponent extends PluginComponent {
    val global: PurePlugin.this.global.type = PurePlugin.this.global
    override val runsAfter = List("refchecks", "uncurry", "annotatePure")
    override val runsBefore = List("checkPurity")
    val phaseName = "showTree"
    def newPhase(_prev: Phase) = new ShowTreePhase(_prev)

    class ShowTreePhase(prev: Phase) extends StdPhase(prev) {
      override def name = phaseName
      def apply(unit: CompilationUnit) {
        global.treeBrowsers.create().browse(unit.body)
      }
    }
  }
}