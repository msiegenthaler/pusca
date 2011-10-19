package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

class RemoveUnnecessaryApplySideEffectComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("typer", "applySideEffect")
  val phaseName = "removeUnnecessaryApplySideEffect"
  def newTransformer(unit: CompilationUnit) = new RemoveUnnecessaryApplySideEffect

  class RemoveUnnecessaryApplySideEffect extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case ApplySideEffect(arg) if !hasAnnotation(arg.tpe, Annotation.sideEffect) ⇒
        transform(arg)
      case a @ ApplySideEffect(arg) ⇒
        super.transform(a)

      case other ⇒ super.transform(other)
    }
  }
}