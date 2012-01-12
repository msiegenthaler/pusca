package pusca.plugin.old

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags

/**
 * Checks that pure classes cannot extend impure ones.
 */
class ConstructorOverrideComponent(val global: Global) extends PluginComponent with PuscaDefinitions {
  import global._

  override val runsAfter = List("typer", "purityDeclarationConflictDetector", "uncurry")
  val phaseName = "constructorOverride"
  def newPhase(prev: Phase) = new ConstructorOverride(prev)

  class ConstructorOverride(prev: Phase) extends StdPhase(prev) {

    override def apply(unit: CompilationUnit) {
      def handle(t: Tree): Unit = {
        t match {
          //TODO handle @impureIf correctly
          case c @ ClassDef(_, name, _, Template(parents, _, _)) if !hasAnnotation(c.symbol, Annotation.impure) ⇒
            val ip = parents.filter(p ⇒ hasAnnotation(p.symbol, Annotation.impure))
            if (!ip.isEmpty) reporter.error(c.pos, "The pure class '" + name + "' has impure parent: '" + ip.mkString(" with ") + "'")

          case _ ⇒ ()
        }
        t.children.foreach(handle)
      }
      handle(unit.body)
    }
  }
}