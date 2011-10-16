package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/**
 * Changes the return type of methods annotated with @impure by adding @sideEffect.
 */
class MethodReturnTypeAnnotatorComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("parser")
  override val runsBefore = List("namer")
  val phaseName = "methodReturnTypeAnnotator"
  def newTransformer(unit: CompilationUnit) = new MethodReturnTypeAnnotator

  class MethodReturnTypeAnnotator extends Transformer {
    private var impureClass: Boolean = false

    override def transform(tree: Tree): Tree = tree match {
      case d: DefDef if hasAnnotation(d, Annotation.impure) && !hasAnnotation(d.tpt, Annotation.sideEffect) ⇒
        val ntpt = annotate(d.tpt, Annotation.sideEffect)
        super.transform(d.copy(tpt = ntpt))
      case d: DefDef if isConstructor(d) && !hasAnnotation(d.tpt, Annotation.sideEffect) && impureClass ⇒
        val ntpt = annotate(d.tpt, Annotation.sideEffect)
        super.transform(d.copy(tpt = ntpt))
      case c: ClassDef ⇒
        impureClass = hasAnnotation(c, Annotation.impure)
        super.transform(c)
      case other ⇒
        super.transform(tree)
    }
  }
}