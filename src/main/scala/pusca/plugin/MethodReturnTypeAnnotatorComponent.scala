package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/**
 * Changes the return type of methods annotated with @impure by adding @sideEffect.
 */
class MethodReturnTypeAnnotatorComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions {
  import global._

  val runsAfter = List("parser")
  override val runsBefore = List("namer")
  val phaseName = "methodReturnTypeAnnotator"
  def newTransformer(unit: CompilationUnit) = new MethodReturnTypeAnnotator

  class MethodReturnTypeAnnotator extends Transformer {
    protected def annotationName(t: Tree): Option[Name] = t match {
      case Apply(Select(New(Ident(tpe)), method), args) ⇒ Some(tpe)
      case _ ⇒ None
    }
    protected def makeAnnotation(annot: Symbol): Tree = {
      Apply(Select(New(Ident(annot.name)), mkTermName("<init>")), Nil)
    }

    protected def sameName(annot: Symbol)(cand: Name) = cand == annot.name
    protected def hasAnnotation(d: DefDef, annot: Symbol): Boolean = {
      d.mods.annotations.find { a ⇒
        val n = annotationName(a)
        n.filter(sameName(annot)).isDefined
      }.isDefined
    }
    protected def hasAnnotation(c: ClassDef, annot: Symbol): Boolean = {
      c.mods.annotations.find { a ⇒
        val n = annotationName(a)
        n.filter(sameName(annot)).isDefined
      }.isDefined
    }
    protected def hasAnnotation(t: Tree, annot: Symbol): Boolean = t match {
      case Annotated(a, t) if annotationName(a).filter(sameName(annot)).isDefined ⇒ true
      case Annotated(a, t) ⇒ hasAnnotation(t, annot)
      case _ ⇒ false
    }
    protected def annotate(t: Tree, a: Symbol): Tree = Annotated(makeAnnotation(a), t)

    protected def isConstructor(d: DefDef) = d.name.toString == "<init>"

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