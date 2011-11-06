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
    //ShowTreeComponent, //
    new PurityCheckerComponent(global),
    new ParametrizedTypesApplicatorComponent(global),
    new ConstructorOverrideComponent(global),
    new MarkMethodReturnPathComponent(global),
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
    //override val runsAfter = List("markMethodReturnPath", "methodReturnTypeAnnotator")
    //override val runsRightAfter = Some("markMethodReturnPath")
    override val runsAfter = List("typer", "forbiddenSideEffectAssignment", "parametrizedTypesApplicator")
    override val runsBefore = List("purityChecker")
    val phaseName = "showTree"
    def newPhase(_prev: Phase) = new ShowTreePhase(_prev)

    private var cnt = 0

    class ShowTreePhase(prev: Phase) extends StdPhase(prev) {
      override def name = phaseName
      def apply(unit: CompilationUnit) {
        cnt = cnt + 1
        if (cnt % 4 == 0) { //customized for the tests
          global.treeBrowsers.create().browse(prev.name, unit :: Nil)
        }
      }
    }
  }
}