package pusca.plugin
import scala.tools.nsc.Global

trait PuscaDefinitions {
  val global: Global
  import global._

  protected object Annotation {
    def apply(annotation: Symbol): AnnotationInfo = AnnotationInfo(annotation.tpe, Nil, Nil)

    val sideEffect = definitions.getClass("pusca.sideEffect")
    val pure = definitions.getClass("pusca.pure")
    val impure = definitions.getClass("pusca.impure")
    val declarePure = definitions.getClass("pusca.declarePure")
  }

  protected def hasAnnotation(tpe: Type, a: Symbol): Boolean = {
    tpe.annotations.find(_.atp.typeSymbol == a).isDefined
  }
  protected def hasAnnotation(tpe: Symbol, a: Symbol): Boolean = {
    tpe.annotations.find(_.atp.typeSymbol == a).isDefined
  }
  protected def annotateWith(tpe: Type, a: Symbol): Type = {
    if (hasAnnotation(tpe, a)) tpe else tpe.withAnnotations(Annotation(a) :: tpe.annotations)
  }

  protected lazy val puscaPackage = definitions.getModule("pusca")
  protected lazy val applySideEffectMethod = definitions.getMember(puscaPackage, "applySideEffect")
  protected lazy val addSideEffectMethod = definitions.getMember(puscaPackage, "addSideEffect")

  object ApplySideEffect {
    def unapply(t: Tree) = t match {
      case Apply(TypeApply(Select(Select(Ident(p), pko), mn), _), arg :: Nil) if p == puscaPackage.name && pko == stringToTermName("package") && mn == applySideEffectMethod.name ⇒
        Some(arg)
      case _ ⇒ None
    }
  }
  object AddSideEffect {
    def unapply(t: Tree) = t match {
      case Apply(TypeApply(Select(Select(Ident(p), pko), mn), _), arg :: Nil) if p == puscaPackage.name && pko == stringToTermName("package") && mn == addSideEffectMethod.name ⇒
        Some(arg)
      case _ ⇒ None
    }
  }

  object PureDefDef {
    def unapply(t: Tree) = t match {
      case d @ DefDef(mods, name, tparams, vparams, tpt, rhs) if !hasAnnotation(tpt.tpe, Annotation.sideEffect) && !d.symbol.isGetter && !d.symbol.isSetter ⇒
        Some(d)
      case _ ⇒ None
    }
  }

}