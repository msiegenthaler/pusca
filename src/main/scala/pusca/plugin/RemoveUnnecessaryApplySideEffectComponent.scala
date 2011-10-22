package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/**
 * Removes all unnecessary calls to applySideEffect (inserted by the applySideEffect phase).
 * The reason they get inserted and then later removed is, that the information to decide whether they were necessary is only
 * available after the typer has run.
 */
class RemoveUnnecessaryApplySideEffectComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("typer", "applySideEffect")
  val phaseName = "removeUnnecessaryApplySideEffect"
  def newTransformer(unit: CompilationUnit) = new RemoveUnnecessaryApplySideEffect

  class RemoveUnnecessaryApplySideEffect extends RemoveUnnecessaryApplySideEffectBase
}