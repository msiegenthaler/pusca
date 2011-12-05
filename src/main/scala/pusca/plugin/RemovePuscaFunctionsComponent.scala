package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/** Removes all calls to pusca functions (addSideEffect, applySideEffect) */
class RemovePuscaFunctionsComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("typer", "purityChecker")
  override val runsBefore = List("tailcalls")
  val phaseName = "removePuscaFunctions"
  def newTransformer(unit: CompilationUnit) = new RemovePuscaFunctions

  class RemovePuscaFunctions extends Transformer {
    override def transform(tree: Tree): Tree = {
      val nt = tree match {
        case ApplySideEffect(f) ⇒ f
        case AddSideEffect(f)   ⇒ f
        case t                  ⇒ t
      }
      super.transform(tree)
    }
  }
}
