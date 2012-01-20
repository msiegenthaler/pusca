package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/**
 * Changes the return type of methods annotated with @impure by adding @sideEffect and adds @impureIfReturnType to unannotated
 * methods.
 */
class MethodReturnTypeAnnotatorComponent(val global: Global) extends PluginComponent with Transform with ParserStageSupport {
  import global._

  val runsAfter = List("parser")
  override val runsBefore = List("namer")
  val phaseName = "methodReturnTypeAnnotator"
  def newTransformer(unit: CompilationUnit) = new MethodReturnTypeAnnotator

  class MethodReturnTypeAnnotator extends Transformer {
    override def transform(tree: Tree): Tree = tree match {

      //annotate all methods without an annotation with @impureIfReturnType
      // (will get replaced with @pure in a later stage if the return type is not a skolem or type parameter)
      //TODO document which stage as soon as it is implemented.
      case d: DefDef if !hasPuscaMethodAnnotation(d) ⇒
        val nmods = d.mods.withAnnotations(makeAnnotation(PurityDecl.impureIfReturnType.annotation) :: d.mods.annotations)
        val ndef = treeCopy.DefDef(d, nmods, d.name, d.tparams, d.vparamss, d.tpt, d.rhs)
        transform(ndef) //process again

      case other ⇒
        super.transform(tree)
    }
  }
}