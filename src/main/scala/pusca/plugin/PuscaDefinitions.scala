package pusca.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags._

trait PuscaDefinitions {
  val global: Global
  import global._

  protected object Annotation {
    def apply(annotation: Symbol): AnnotationInfo = AnnotationInfo(annotation.tpe, Nil, Nil)

    val sideEffect = definitions.getClass("pusca.sideEffect")
    val pure = definitions.getClass("pusca.pure")
    val impure = definitions.getClass("pusca.impure")
    val impureIf = definitions.getClass("pusca.impureIf")
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
  protected def removeAnnotation(tpe: Type, a: Symbol): Type = {
    tpe.withAnnotations(tpe.annotations.filterNot(_.atp.typeSymbol == a))
  }

  protected lazy val puscaPackage = definitions.getModule("pusca")
  protected lazy val packageObject = stringToTermName("package")
  protected lazy val applySideEffectMethod = definitions.getMember(puscaPackage, "applySideEffect")
  protected lazy val addSideEffectMethod = definitions.getMember(puscaPackage, "addSideEffect")
  protected lazy val markReturnValueMethod = definitions.getMember(puscaPackage, "__internal__markReturnValue")
  protected lazy val markReturnValueWithSideEffectMethod = definitions.getMember(puscaPackage, "__internal__markReturnValueWithSideEffect")

  def hasSideEffect(t: Type) = hasAnnotation(t, Annotation.sideEffect)

  object ApplySideEffect {
    def unapply(t: Tree) = t match {
      case Apply(TypeApply(Select(Select(Ident(p), pko), mn), _), arg :: Nil) if p == puscaPackage.name && pko == packageObject && mn == applySideEffectMethod.name ⇒
        Some(arg)
      case Apply(Select(Select(Ident(p), pko), mn), arg :: Nil) if p == puscaPackage.name && pko == packageObject && mn == applySideEffectMethod.name ⇒
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
  object MarkReturnValue {
    def unapply(t: Tree) = t match {
      case Apply(TypeApply(Select(Select(Ident(p), pko), mn), _), arg :: Nil) if p == puscaPackage.name && pko == packageObject && mn == markReturnValueMethod.name ⇒
        Some(arg, false)
      case Apply(Select(Select(Ident(p), pko), mn), arg :: Nil) if p == puscaPackage.name && pko == packageObject && mn == markReturnValueMethod.name ⇒
        Some(arg, false)
      case Apply(TypeApply(Select(Select(Ident(p), pko), mn), _), arg :: Nil) if p == puscaPackage.name && pko == packageObject && mn == markReturnValueWithSideEffectMethod.name ⇒
        Some(arg, true)
      case Apply(Select(Select(Ident(p), pko), mn), arg :: Nil) if p == puscaPackage.name && pko == packageObject && mn == markReturnValueWithSideEffectMethod.name ⇒
        Some(arg, true)
      case _ ⇒ None
    }
  }

  protected def applySideEffectFun = Select(Ident(puscaPackage.name.toTermName), "applySideEffect")
  protected def applySideEffect(v: Tree) = {
    val a = Apply(applySideEffectFun, v :: Nil)
    a.symbol.setFlag(SYNTHETIC)
    a.pos = v.pos
    a
  }
  protected def addSideEffectFun = Select(Ident(puscaPackage.name.toTermName), "addSideEffect")
  protected def addSideEffect(v: Tree) = {
    val a = Apply(addSideEffectFun, v :: Nil)
    a.symbol.setFlag(SYNTHETIC)
    a.pos = v.pos
    a
  }

  object Purity {
    sealed trait Purity
    case object AlwaysPure extends Purity
    case object AlwaysImpure extends Purity
    case class ImpureDependingOn(tparams: Set[String]) extends Purity

    /** Determines the declared purity of a method. Does not take the body of the method into account, whether the body conforms to the declaration is checked elsewhere. */
    def purityOf(s: Symbol): Purity = s match {
      case s if s.isSetter                          ⇒ AlwaysImpure //setter of var
      case s if s.isGetter && !s.isStable           ⇒ AlwaysImpure //getter on var
      case s if hasAnnotation(s, Annotation.pure)   ⇒ AlwaysPure
      case s if hasAnnotation(s, Annotation.impure) ⇒ AlwaysImpure
      case s if hasAnnotation(s, Annotation.impureIf) ⇒
        val impureIfs = s.annotations.find(_.atp.typeSymbol == Annotation.impureIf) match {
          case Some(AnnotationInfo(_, args, _)) ⇒ args.collect { case SymbolApply(arg) ⇒ arg }
          case None                             ⇒ Nil
        }
        ImpureDependingOn(impureIfs.toSet)
      case s: MethodSymbol ⇒ //impureIfImpureResult
        val rt = resultType(s)
        if (rt.typeSymbol.isTypeParameterOrSkolem) ImpureDependingOn(Set(rt.typeSymbol.name.toString))
        else AlwaysPure
      case _ ⇒ AlwaysPure
    }
    private def resultType(t: MethodSymbol): Type = {
      def resTpe(t: Type): Type = t match {
        case PolyType(_, rt)      ⇒ resTpe(rt)
        case MethodType(_, r)     ⇒ r
        case NullaryMethodType(r) ⇒ r
      }
      resTpe(t.tpe)
    }
    private object SymbolApply {
      private val applyName = stringToTermName("apply")
      private val symbolName = stringToTermName("Symbol")
      private val scalaName = stringToTermName("scala")
      def unapply(t: Tree) = t match {
        case Apply(Select(Select(Ident(scalaName), symbolName), applyName), Literal(arg @ Constant(_)) :: Nil) if arg.tag == StringTag ⇒
          Some(arg.stringValue.intern)
        case _ ⇒ None
      }
    }

    /**
     * Gets the values used for the type parameters of a method.
     * Example:
     *  for
     *  <code>
     * 	trait Example[B] {
     * 		def method1[A](a: A): B
     *  }
     *  new Example[String].method1(10)
     *  </code>
     * it returns: A -> Int, B -> String
     */
    def resolveTypeParams(a: Apply): Map[String, Type] = {
      def methodOwnerTypeParams(a: Tree): Map[String, Type] = a match {
        case Select(o, _) ⇒
          o.tpe.underlying match {
            case TypeRef(p, s, a) ⇒
              s.typeParams.map(_.name.toString).zip(a).toMap
            case _ ⇒ Map()
          }
        case _ ⇒ Map()
      }
      a.fun match {
        case TypeApply(fun, targs) ⇒
          val pl = fun.symbol.typeParams.map(_.name.toString).toList
          methodOwnerTypeParams(fun) ++ pl.zip(targs.map(_.tpe)).toMap
        case f ⇒ methodOwnerTypeParams(f)
      }
    }

    /** checks whether an apply is pure given the list of with names of type parameters that are allowed to be impure. */
    def violatesPurity(a: Apply, allowedImpures: Set[String] = Set()): Boolean = purityOf(a.fun.symbol) match {
      case AlwaysPure   ⇒ false
      case AlwaysImpure ⇒ true
      case ImpureDependingOn(di) ⇒
        def mayHaveSideEffect(tpe: Type) = hasSideEffect(tpe) || tpe.typeSymbol.isTypeParameterOrSkolem
        def isAllowedImpure(tpe: Type) = tpe.typeSymbol.isTypeParameterOrSkolem && allowedImpures.contains(tpe.typeSymbol.name.toString)

        val tparams = resolveTypeParams(a).filter(e ⇒ di.contains(e._1))
        di.filterNot(tparams.contains).foreach { p ⇒ reporter.error(a.pos, "unresolved type parameter " + p + " on call to " + a.fun.symbol.fullName) }
        val ips = tparams.filter(e ⇒ mayHaveSideEffect(e._2) && !isAllowedImpure(e._2))
        ips.nonEmpty
    }
  }

  object PurityChecker {
    case class Error(pos: Position, msg: String) {
      def report = reporter.error(pos, msg)
    }

    import Purity._
    def apply(d: DefDef): List[Error] = apply(d.symbol, "method " + d.symbol.fullName, d.rhs)
    def apply(c: ClassDef): List[Error] = apply(c.symbol, "class " + c.symbol.fullName, c.impl)
    def apply(f: Function): List[Error] = apply(f.symbol, "function " + f.symbol.fullName, f.body)
    def apply(obj: Symbol, objName: String, content: Tree): List[Error] = {
      purityOf(obj) match {
        case AlwaysPure                 ⇒ handle(obj, objName, Set.empty)(content)
        case AlwaysImpure               ⇒ Nil
        case ImpureDependingOn(impures) ⇒ handle(obj, objName, impures)(content)
      }
    }

    private[this] def handle(obj: Symbol, objName: String, allowedImpures: Set[String])(t: Tree, soFar: List[Error] = Nil): List[Error] = t match {
      case a: Apply if violatesPurity(a, allowedImpures) ⇒
        if (a.fun.symbol.isSetter) Error(a.pos, "write to non-local var " + a.fun.symbol.name.toString.dropRight(4) + " inside the pure " + objName) :: soFar
        else if (a.fun.symbol.isGetter) Error(a.pos, "access to non-local var " + a.fun.symbol.name + " inside the pure " + objName) :: soFar
        else Error(a.pos, "impure method call to " + a.fun.symbol.name + " inside the pure " + objName) :: soFar

      case a @ Assign(lhs, rhs) if (!lhs.symbol.ownerChain.contains(obj)) ⇒ // assign to var outside the scope of this method
        Error(a.pos, "write to non-local var " + lhs.symbol.name + " inside the pure " + objName) :: soFar
      case s: Select if s.symbol.isSetter ⇒ //assign to var via setter
        Error(s.pos, "write to non-local var " + s.name.toString.dropRight(4) + " inside the pure " + objName) :: soFar

      case s: Select if s.symbol.isMutable && !s.symbol.ownerChain.contains(obj) ⇒ // read of var outside the scope of this method (private[this])
        Error(s.pos, "access to non-local var " + s.name + " inside the pure " + objName) :: soFar
      case i: Ident if i.symbol.isMutable && !i.symbol.ownerChain.contains(obj) ⇒ // read of var defined in an outer function
        Error(i.pos, "access to non-local var " + i.name + " inside the pure " + objName) :: soFar
      case s: Select if s.symbol.isGetter && !s.symbol.isStable ⇒ // read of var via accessor
        Error(s.pos, "access to non-local var " + s.name + "inside the pure " + objName) :: soFar

      case d: DefDef   ⇒ soFar
      case c: ClassDef ⇒ soFar
      case f: Function ⇒ soFar
      case other       ⇒ other.children.foldLeft(soFar)((sf, e) ⇒ handle(obj, objName, allowedImpures)(e, sf))
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