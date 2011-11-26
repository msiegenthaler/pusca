package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags

/**
 * Checks that PureFunctions are really pure.
 */
class ImpureFunctionComponent(val global: Global) extends PluginComponent with PuscaDefinitions {
  import global._

  override val runsAfter = List("typer")
  override val runsBefore = List("uncurry")
  val phaseName = "impureFunction"
  def newPhase(prev: Phase) = new ConstructorOverride(prev)

  class ConstructorOverride(prev: Phase) extends StdPhase(prev) {

    override def apply(unit: CompilationUnit) {
      def handle(t: Tree): Unit = {
        t match {
          case t: TypeTree ⇒
            val tpe = t.tpe.normalize
            if (hasAnnotation(tpe, Annotation.pureFun) && funResultType(tpe).filter(hasSideEffect).isDefined)
              reporter.error(t.pos, "PureFunction mustn't have a side effect")
          case _ ⇒ ()
        }
        t.children.foreach(handle)
      }
      handle(unit.body)
    }
  }
}