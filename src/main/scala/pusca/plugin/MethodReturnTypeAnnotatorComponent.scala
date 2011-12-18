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
      case Constructor(d) ⇒
        //TODO Handle impure classes
        super.transform(d)

      case d: DefDef if !hasPuscaMethodAnnotation(d) ⇒
        //annotate all methods without an annotation with @impureIfReturnType
        val nmods = d.mods.withAnnotations(makeAnnotation(Annotation.impureIfReturnType) :: d.mods.annotations)
        val ndef = treeCopy.DefDef(d, nmods, d.name, d.tparams, d.vparamss, d.tpt, d.rhs)
        transform(ndef) //process again

      //return type is inferred
      case d @ DefDef(_, _, _, _, TypeTree(), rhs) ⇒ //no return type declared, so mark the return path
        val ndef = {
          d match {
            case d: DefDef if hasAnnotation(d, Annotation.pure) ⇒ Some(MarkSideEffectFree)
            case d: DefDef if hasAnnotation(d, Annotation.impure) ⇒ Some(MarkSideEffect)
            case _ ⇒ None
          }
        }.map { m ⇒
          val nrhs = MarkMethod(m)(rhs)
          treeCopy.DefDef(d, d.mods, d.name, d.tparams, d.vparamss, TypeTree(), nrhs)
        }.getOrElse(d)
        super.transform(ndef)

      //annotate the return type with @sideEffect or @sideEffectFree
      case d: DefDef if hasAnnotation(d, Annotation.impureIf) ⇒
        //TODO transform to impureIfReturnType, if the names match
        super.transform(d)
      case d: DefDef if hasAnnotation(d, Annotation.impureIfReturnType) ⇒
        //TODO
        super.transform(d)
      case d: DefDef if hasAnnotation(d, Annotation.impure) && !hasAnnotation(d.tpt, Annotation.sideEffect) ⇒
        val ntpt = annotate(d.tpt, Annotation.sideEffect)
        val ndef = treeCopy.DefDef(d, d.mods, d.name, d.tparams, d.vparamss, ntpt, d.rhs)
        super.transform(ndef)
      case d: DefDef if hasAnnotation(d, Annotation.pure) && !hasAnnotation(d.tpt, Annotation.sideEffectFree) ⇒
        val ntpt = annotate(d.tpt, Annotation.sideEffectFree)
        val nrhs = MarkMethod(MarkSideEffectFree)(d.rhs) //may cause impure to be 'casted' to pure, but the PurityChecker will find out 
        val ndef = treeCopy.DefDef(d, d.mods, d.name, d.tparams, d.vparamss, ntpt, nrhs)
        super.transform(ndef)

      //don't touch empty function (i.e. val f = doIt _)
      case f @ Function(_, EmptyTree) ⇒
        super.transform(f)
      //annotate the return values of anon functions with infere
      case f @ Function(vp, body) ⇒
        val nb = MarkMethod(MarkInfere)(f.body)
        val nf = treeCopy.Function(f, vp, nb)
        super.transform(nf)

      //TODO also handle conflicts? @pure def a: Int @sideEffect

      //TODO check for the annotations in different places than type parameters and method returns and error them.

      case other ⇒
        super.transform(tree)
    }
  }
}