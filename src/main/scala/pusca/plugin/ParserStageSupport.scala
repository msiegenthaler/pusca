package pusca.plugin
import scala.tools.nsc.Global

private[plugin] trait ParserStageSupport {
  val global: Global
  import global._

  private def sameName(annot: Symbol)(cand: Name) = cand == annot.name
  private def annotationName(t: Tree): Option[Name] = t match {
    case Apply(Select(New(Ident(tpe)), method), args) ⇒ Some(tpe)
    case _ ⇒ None
  }
  private def findAnnotation(a: Symbol, annots: List[Tree]) = annots.view.map(annotationName).flatten.find(sameName(a)).isDefined

  protected def hasAnnotation(d: DefDef, annot: Symbol): Boolean = findAnnotation(annot, d.mods.annotations)
  protected def hasAnnotation(c: ClassDef, annot: Symbol): Boolean = findAnnotation(annot, c.mods.annotations)
  protected def hasAnnotation(t: Tree, annot: Symbol): Boolean = t match {
    case Annotated(a, t) if findAnnotation(annot, List(a)) ⇒ true
    case Annotated(a, t) ⇒ hasAnnotation(t, annot)
    case _ ⇒ false
  }

  protected def makeAnnotation(annot: Symbol): Tree = {
    Apply(Select(New(Ident(annot.name)), mkTermName("<init>")), Nil)
  }
  protected def annotate(t: Tree, a: Symbol): Tree = Annotated(makeAnnotation(a), t)

  protected def deannotate(t: Tree, a: Symbol): Tree = t match {
    case Annotated(annot, t) ⇒
      val nt = deannotate(t, a)
      if (annotationName(annot).filter(sameName(a)).isDefined) nt
      else Annotated(annot, t)
    case _ ⇒ t
  }

  protected def isConstructor(d: DefDef) = d.name.toString == "<init>"
}