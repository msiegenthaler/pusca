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
//    ShowTreeComponent, //
    new RemoveInferedSideEffectFromValComponent(global),
    new RemoveUnnecessaryApplySideEffectComponent(global),
    new DeclarationConflictDetectorComponent(global),
    new UnnecessaryDeclarePureComponent(global),
    new ForbiddenSideEffectAssignmentComponent(global),
    new MethodReturnTypeAnnotatorComponent(global))

  val checker = new SideEffectChecker {
    val global: PurePlugin.this.global.type = PurePlugin.this.global
  }
  global.addAnnotationChecker(checker.checker)

  import global._
  private object ShowTreeComponent extends PluginComponent {
    val global: PurePlugin.this.global.type = PurePlugin.this.global
    override val runsAfter = List("removeInferedSideEffectFromVal")
    override val runsRightAfter = Some("removeInferedSideEffectFromVal")
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