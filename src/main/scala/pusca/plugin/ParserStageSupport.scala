package pusca.plugin
import scala.tools.nsc.Global

private[plugin] trait ParserStageSupport extends PuscaDefinitions {
  val global: Global
  import global._

  private def sameName(annot: Symbol)(cand: Name) = cand == annot.name
  protected def annotationName(t: Tree): Option[Name] = t match {
    case Apply(Select(New(Ident(tpe)), method), args) ⇒ Some(tpe)
    case Apply(Select(New(Select(Ident(pkg), tpe)), method), args) ⇒ Some(tpe)
    case _ ⇒ None
  }
  protected def findAnnotation(a: Symbol, annots: List[Tree]) = annots.view.map(annotationName).flatten.find(sameName(a))
  protected def findAnnotation(a: List[Symbol], annots: List[Tree]) = annots.view.map(annotationName).flatten.find(v ⇒ a.find(sameName(_)(v)).isDefined)

  protected def hasAnnotation(d: DefDef, annot: Symbol): Boolean = findAnnotation(annot, d.mods.annotations).isDefined
  protected def hasAnnotation(c: ClassDef, annot: Symbol): Boolean = findAnnotation(annot, c.mods.annotations).isDefined
  protected def hasAnnotation(t: Tree, annot: Symbol): Boolean = t match {
    case Annotated(a, t) if findAnnotation(annot, List(a)).isDefined ⇒ true
    case Annotated(a, t) ⇒ hasAnnotation(t, annot)
    case _ ⇒ false
  }
  protected def hasPuscaMethodAnnotation(d: DefDef) = findAnnotation(Annotation.allForMethod, d.mods.annotations).isDefined
  protected def hasPuscaMethodAnnotation(c: ClassDef) = findAnnotation(Annotation.allForMethod, c.mods.annotations).isDefined

  protected def makeAnnotation(annot: Symbol): Tree = {
    Apply(Select(New(Select(Ident("pusca"), annot.name)), mkTermName("<init>")), Nil)
  }
  protected def annotate(t: Tree, a: Symbol): Tree = Annotated(makeAnnotation(a), t)

  protected def deannotate(t: Tree, a: Symbol): Tree = t match {
    case Annotated(annot, t) ⇒
      val nt = deannotate(t, a)
      if (annotationName(annot).filter(sameName(a)).isDefined) nt
      else Annotated(annot, t)
    case _ ⇒ t
  }

  object Constructor {
    def unapply(d: DefDef) = {
      if (d.name.toString == "<init>") Some(d)
      else None
    }
  }
}