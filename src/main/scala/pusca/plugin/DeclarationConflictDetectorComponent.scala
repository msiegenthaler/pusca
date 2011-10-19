package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/**
 * Gives a compilation error if there are conflicting annotations on methods (i.e. @pure and @sideEffect).
 */
class DeclarationConflictDetectorComponent(val global: Global) extends PluginComponent with PuscaDefinitions {
  import global._

  val runsAfter = List("typer")
  val phaseName = "purityDeclarationConflictDetector"
  def newPhase(prev: Phase) = new DeclarationConflictDetector(prev)

  class DeclarationConflictDetector(prev: Phase) extends StdPhase(prev) {
    private def isAnnotPure(s: DefDef) = hasAnnotation(s.symbol, Annotation.pure)
    override def name = phaseName

    override def apply(unit: CompilationUnit) {
      def handle(t: Tree): Unit = {
        t match {
          case d: DefDef if isAnnotPure(d) && hasAnnotation(d.tpt.tpe, Annotation.sideEffect) ⇒
            reporter.error(d.pos, "method '" + d.name + "' has @pure annotation and a @sideEffect return type")
          case _ ⇒ ()
        }
        t.children.foreach(handle)
      }
      handle(unit.body)
    }
  }
}