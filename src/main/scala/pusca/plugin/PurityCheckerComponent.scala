package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags

/**
 * Checks for applySideEffect calls inside pure methods and yields compiler error if they are found.
 */
class PurityCheckerComponent(val global: Global) extends PluginComponent with PuscaDefinitions {
  import global._

  override val runsAfter = List("typer", "removeUnnecessaryApplySideEffect", "forbiddenSideEffectAssignment", "purityDeclarationConflictDetector", "uncurry")
  val phaseName = "purityChecker"
  def newPhase(prev: Phase) = new PurityChecker(prev)

  class PurityChecker(prev: Phase) extends StdPhase(prev) {

    override def apply(unit: CompilationUnit) {
      def handle(t: Tree): Unit = t match {
        case PureDefDef(d) ⇒
          PureMethodChecker(d.symbol, "method '" + d.symbol.fullName + "'", d.rhs).foreach(_.report)
          handle(d.rhs)
        case PureFunction(f) ⇒
          PureMethodChecker(f.symbol, "function '" + f.symbol.fullName + "'", f.body).foreach(_.report)
          handle(f.body)
        case c @ PureConstructor(tmpl) ⇒
          PureMethodChecker(tmpl.symbol, "class '" + c.symbol.fullName + "'", tmpl).foreach(_.report)
          handle(tmpl)
        case other ⇒
          other.children.foreach(handle)
      }
      handle(unit.body)
    }
  }
}