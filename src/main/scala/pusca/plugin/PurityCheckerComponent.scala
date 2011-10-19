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

  val runsAfter = List("typer", "removeUnnecessaryApplySideEffect", "forbiddenSideEffectAssignment", "purityDeclarationConflictDetector")
  val phaseName = "purityChecker"
  def newPhase(prev: Phase) = new PurityChecker(prev)

  class PurityChecker(prev: Phase) extends StdPhase(prev) {

    override def apply(unit: CompilationUnit) {
      def handle(t: Tree): Unit = t match {
        case ImpureDefDef(d) ⇒
          handlePureMethod(d.symbol)(d.rhs)
        case other ⇒
          other.children.foreach(handle)
      }
      def handlePureMethod(fun: Symbol)(t: Tree): Unit = t match {
        case a @ ApplySideEffect(impure) ⇒
          reporter.error(a.pos, "impure method call inside the pure method '" + fun.fullName + "'")
        case a @ Assign(lhs, rhs) if (!lhs.symbol.ownerChain.contains(fun)) ⇒ // assign to var outside the scope of this method
          reporter.error(a.pos, "write to non-local var inside the pure method '" + fun.fullName + "'")
        case s: Select if s.symbol.isMutable && !s.symbol.ownerChain.contains(fun) ⇒ // read of var outside the scope of this method
          reporter.error(s.pos, "access to non-local var inside the pure method '" + fun.fullName + "'")

        case d: DefDef   ⇒ handle(d)
        case c: ClassDef ⇒ handle(c)
        case other       ⇒ other.children.foreach(handlePureMethod(fun))
      }
      handle(unit.body)
    }
  }
}