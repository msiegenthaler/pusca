package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags

/**
 * Checks for calls to impure methods and raises compilations errors if found.
 */
class PurityCheckerComponent(val global: Global) extends PluginComponent with PuscaDefinitions {
  import global._

  override val runsAfter = List("typer", "forbiddenSideEffectAssignment", "purityDeclarationConflictDetector", "uncurry")
  override val runsBefore = List("tailcalls")
  val phaseName = "purityChecker"
  def newPhase(prev: Phase) = new PurityChecker(prev)

  class PurityChecker(prev: Phase) extends StdPhase(prev) {
    override def apply(unit: CompilationUnit) {
      def look(t: Tree): Unit = {
        t match {
          case d: DefDef   ⇒ PurityChecker(d).foreach(_.report)
          case c: ClassDef ⇒ PurityChecker(c).foreach(_.report)
          case f: Function ⇒ PurityChecker(f).foreach(_.report)
          case _           ⇒ ()
        }
        t.children.foreach(look)
      }
      look(unit.body)
    }
  }
}