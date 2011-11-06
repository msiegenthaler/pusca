package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/**
 * Adds applySideEffect to everything that results in a type parameter that might be @sideEffect.
 */
class ParametrizedTypesApplicatorComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("typer")
  val phaseName = "parametrizedTypesApplicator"
  def newTransformer(unit: CompilationUnit) = new ParametizedTypesApplicator

  class ParametizedTypesApplicator extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case other ⇒ super.transform(other)
    }
  }
}