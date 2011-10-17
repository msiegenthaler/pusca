package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

class RemoveUnnecessaryApplySideEffectComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("typer", "removeInferedSideEffectFromVal")
  val phaseName = "removeUnnecessaryApplySideEffect"
  def newTransformer(unit: CompilationUnit) = new RemoveUnnecessaryApplySideEffect

  class RemoveUnnecessaryApplySideEffect extends Transformer {
    protected val puscaPackage = definitions.getModule("pusca")
    protected val applySideEffectMethod = definitions.getMember(puscaPackage, "applySideEffect")

    object ApplySideEffect {
      def unapply(t: Tree) = t match {
        case Apply(TypeApply(Select(Select(Ident(p), pko), mn), _), arg :: Nil) if p == puscaPackage.name && pko == stringToTermName("package") && mn == applySideEffectMethod.name ⇒
          Some(arg)
        case _ ⇒ None
      }
    }

    override def transform(tree: Tree): Tree = tree match {
      case ApplySideEffect(arg) if !hasAnnotation(arg.tpe, Annotation.sideEffect) ⇒
        transform(arg)
      case a @ ApplySideEffect(arg) ⇒
        super.transform(a)

      case other ⇒ super.transform(other)
    }
  }
}