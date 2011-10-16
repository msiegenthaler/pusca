package pusca.plugin
import scala.tools.nsc.Global

trait ParserStageSupport {
  val global: Global
  import global._
  
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
}