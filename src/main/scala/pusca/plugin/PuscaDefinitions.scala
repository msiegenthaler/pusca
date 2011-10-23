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
  object PureFunction {
    def unapply(t: Tree) = t match {
      case f @ Function(vparams, body) if !hasAnnotation(body.symbol, Annotation.sideEffect) ⇒
        Some(f)
      case _ ⇒ None
    }
  }
  object PureConstructor {
    def unapply(t: Tree) = t match {
      case c @ ClassDef(mod, name, tparams, impl) if !hasAnnotation(c.symbol, Annotation.impure) ⇒
        Some(impl)
      case _ ⇒ None
    }
  }

  object PureMethodChecker {
    case class Error(pos: Position, msg: String) {
      def report = reporter.error(pos, msg)
    }

    def apply(obj: Symbol, objName: String, t: Tree): List[Error] = handle(obj, objName)(t, Nil)

    private[this] def handle(obj: Symbol, objName: String)(t: Tree, soFar: List[Error]): List[Error] = t match {
      case a @ ApplySideEffect(impure) ⇒
        Error(a.pos, "impure method call inside the pure " + objName) :: soFar

      case a @ Assign(lhs, rhs) if (!lhs.symbol.ownerChain.contains(obj)) ⇒ // assign to var outside the scope of this method
        Error(a.pos, "write to non-local var inside the pure " + objName) :: soFar
      case s: Select if s.symbol.isSetter ⇒ //assign to var via setter
        Error(s.pos, "write to non-local var inside the pure " + objName) :: soFar

      case s: Select if s.symbol.isMutable && !s.symbol.ownerChain.contains(obj) ⇒ // read of var outside the scope of this method (private[this])
        Error(s.pos, "access to non-local var inside the pure " + objName) :: soFar
      case i: Ident if i.symbol.isMutable && !i.symbol.ownerChain.contains(obj) ⇒ // read of var defined in an outer function
        Error(i.pos, "access to non-local var inside the pure " + objName) :: soFar
      case s: Select if s.symbol.isGetter && !s.symbol.isStable ⇒ // read of var via accessor
        Error(s.pos, "access to non-local var inside the pure " + objName) :: soFar

      case d: DefDef   ⇒ soFar
      case c: ClassDef ⇒ soFar
      case f: Function ⇒ soFar
      case other       ⇒ other.children.foldLeft(soFar)((sf, e) ⇒ handle(obj, objName)(e, sf))
    }
  }

  trait RemoveUnnecessaryApplySideEffectBase extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      //remove applySideEffect
      case ApplySideEffect(arg) if !hasAnnotation(arg.tpe, Annotation.sideEffect) ⇒
        transform(arg)

      //remove addSideEffect
      case AddSideEffect(arg) if hasAnnotation(arg.tpe, Annotation.sideEffect) ⇒
        transform(arg)

      case other ⇒ super.transform(other)
    }
  }

}