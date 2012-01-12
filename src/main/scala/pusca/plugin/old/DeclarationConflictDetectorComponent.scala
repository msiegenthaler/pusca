package pusca.plugin.old

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/**
 * Gives a compilation error if there are conflicting annotations on methods (i.e. @pure and @sideEffect or @pure and @impure) or
 * annotations are used in the wrong place.
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
          case d: DefDef if Annotation.allForMethod.filter(a ⇒ hasAnnotation(d.symbol, a)).size > 1 ⇒
            reporter.error(d.pos, "method " + d.name + " has multiple purity annotations")
          case d: DefDef ⇒ Annotation.allForMethod.find(a ⇒ hasAnnotation(d.tpt.tpe, a)) match {
            case Some(a) ⇒ reporter.error(d.pos, "purity annotation @" + a.name + " can only be used on a method, not on the return type")
            case None    ⇒ ()
          }

          case _ ⇒ ()
        }
        t.children.foreach(handle)
      }
      handle(unit.body)
    }
  }
}