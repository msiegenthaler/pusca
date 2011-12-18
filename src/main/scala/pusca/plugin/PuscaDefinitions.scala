package pusca.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags._
import scala.annotation.tailrec

trait PuscaDefinitions {
  val global: Global
  import global._

  protected object Annotation {
    def apply(annotation: Symbol): AnnotationInfo = AnnotationInfo(annotation.tpe, Nil, Nil)

    val pure = definitions.getClass("pusca.pure")
    val impure = definitions.getClass("pusca.impure")
    val impureIf = definitions.getClass("pusca.impureIf")
    val impureIfReturnType = definitions.getClass("pusca.impureIfReturnType")
    val declarePure = definitions.getClass("pusca.declarePure")

    val sideEffect = definitions.getClass("pusca.sideEffect")
    val sideEffectFree = definitions.getClass("pusca.sideEffectFree")

    val returnedInfere = definitions.getClass("pusca.Internal.returnedInfere")
    val returnedSideEffect = definitions.getClass("pusca.Internal.returnedSideEffect")
    val returnedSideEffectFree = definitions.getClass("pusca.Internal.returnedSideEffectFree")

    val allForMethod = pure :: impure :: impureIf :: impureIfReturnType :: declarePure :: Nil
    val allForTypes = sideEffect :: sideEffectFree :: Nil
    val internalForTypes = returnedInfere :: returnedSideEffect :: returnedSideEffectFree :: Nil
  }

  //TODO deprecate (use tpe.hasAnnotation)
  protected def hasAnnotation(tpe: Type, a: Symbol): Boolean = {
    tpe.annotations.find(_.atp.typeSymbol == a).isDefined
  }
  //TODO deprecate (use tpe.hasAnnotation)
  protected def hasAnnotation(tpe: Symbol, a: Symbol): Boolean = {
    tpe.annotations.find(_.atp.typeSymbol == a).isDefined
  }
  protected def annotateWith(tpe: Type, a: Symbol): Type = {
    if (hasAnnotation(tpe, a)) tpe else tpe.withAnnotations(Annotation(a) :: tpe.annotations)
  }
  protected def removeAnnotation(tpe: Type, a: Symbol): Type = {
    tpe.withoutAnnotations.withAnnotations(tpe.annotations.filterNot(_.atp.typeSymbol == a))

  }

  //TODO still needed and correct?
  object AlwaysPureType {
    def unapply(t: Type) = {
      if (t.typeSymbol.isTypeParameterOrSkolem) {
        if (t.hasAnnotation(Annotation.sideEffectFree)) Some(())
        else None
      } else if (t.hasAnnotation(Annotation.sideEffect)) None
      else Some(())
    }
  }

  /** A side-effect free type */
  object SideEffectFreeType {
    def unapply(t: Type) = {
      if (t.dealias.hasAnnotation(Annotation.sideEffectFree)) Some(t)
      else None
    }
  }
  /** A type with side effect */
  object SideEffectType {
    def unapply(t: Type) = {
      if (t.dealias.hasAnnotation(Annotation.sideEffect)) Some(t)
      else None
    }
  }
  /** A type with neither @sideEffect and @sideEffectFree */
  object UnspecifiedSideEffectType {
    def unapply(t: Type) = {
      if (!t.dealias.hasAnnotation(Annotation.sideEffect) && !t.dealias.hasAnnotation(Annotation.sideEffectFree)) Some(t)
      else None
    }
  }
  /** A type on the return path, that has been marked by MethodReturnTypeAnnotatorComponent */
  object MarkReturnType {
    def unapply(t: Type) = t match {
      case t if t.hasAnnotation(Annotation.returnedInfere) ⇒ Some(MarkInfere)
      case t if t.hasAnnotation(Annotation.returnedSideEffect) ⇒ Some(MarkSideEffect)
      case t if t.hasAnnotation(Annotation.returnedSideEffectFree) ⇒ Some(MarkSideEffectFree)
      case _ ⇒ None
    }
  }

  protected sealed trait Mark
  protected object MarkInfere extends Mark
  protected object MarkSideEffect extends Mark
  protected object MarkSideEffectFree extends Mark

  object MarkMethod {
    def unapply(s: Symbol) = s match {
      case this.markInfereMethod         ⇒ Some(MarkInfere)
      case this.markSideEffectMethod     ⇒ Some(MarkSideEffect)
      case this.markSideEffectFreeMethod ⇒ Some(MarkSideEffectFree)
      case _                             ⇒ None
    }
    def unapply(t: Tree): Option[(Mark, Tree)] = t match {
      case a @ Apply(fun, arg :: Nil) ⇒
        unapply(fun.symbol).map(m ⇒ (m, arg))
      case _ ⇒ None
    }

    private lazy val puscaPackage = definitions.getModule("pusca")
    private lazy val puscaInternalObject = definitions.getMember(puscaPackage, "Internal")
    private lazy val markInfereMethod = definitions.getMember(puscaInternalObject, "markInfere")
    private lazy val markSideEffectMethod = definitions.getMember(puscaInternalObject, "markSideEffect")
    private lazy val markSideEffectFreeMethod = definitions.getMember(puscaInternalObject, "markSideEffectFree")

    def apply(mark: Mark)(fun: Tree): Tree = {
      val a = Apply(markFunFor(mark), fun :: Nil)
      a.symbol.setFlag(SYNTHETIC)
      a.pos = fun.pos
      a
    }
    private[this] def markFun(name: String) = Select(Select(Ident("pusca"), "Internal"), name)
    private def markInfere = markFun("markInfere")
    private def markSideEffect = markFun("markSideEffect")
    private def markSideEffectFree = markFun("markSideEffectFree")
    private def markFunFor(mark: Mark) = mark match {
      case MarkInfere         ⇒ markInfere
      case MarkSideEffect     ⇒ markSideEffect
      case MarkSideEffectFree ⇒ markSideEffectFree
    }
  }

  object Purity {
    sealed trait Purity
    case object AlwaysPure extends Purity
    case object AlwaysImpure extends Purity
    case object DeclaredPure extends Purity
    case class ImpureDependingOn(tparams: Set[String]) extends Purity

    /**
     * Determines the declared purity of a method/class/function.
     * Does not take the body of the method into account, whether the body conforms to the declaration is checked elsewhere.
     */
    def purityOf(s: Symbol): Purity = {
      val r = s match {
//        case MarkMethod(_) ⇒
//          println("found mark method " + s)
//          AlwaysPure
        case s if s.isSetter                               ⇒ AlwaysImpure //setter of var
        case s if s.isGetter && !s.isStable                ⇒ AlwaysImpure //getter on var
        case s if s.isGetter                               ⇒ AlwaysPure //  getter on val
        case s if s.isConstructor                          ⇒ purityOf(s.owner)
        case s if hasAnnotation(s, Annotation.pure)        ⇒ AlwaysPure
        case s if hasAnnotation(s, Annotation.declarePure) ⇒ DeclaredPure
        case s if hasAnnotation(s, Annotation.impure)      ⇒ AlwaysImpure
        case s if hasAnnotation(s, Annotation.impureIf) ⇒
//          println("impureIf for " + s)
          val impureIfs = s.annotations.find(_.atp.typeSymbol == Annotation.impureIf) match {
            case Some(AnnotationInfo(_, args, _)) ⇒ args.collect { case SymbolApply(arg) ⇒ arg }
            case None                             ⇒ Nil
          }
          ImpureDependingOn(impureIfs.toSet)
        case s: MethodSymbol if s.hasAnnotation(Annotation.impureIfReturnType) ⇒
//          println("impureIfReturnType for " + s)
          purityOfType(resultType(s))
        case s: MethodSymbol ⇒ //method that was not processed with pusca
          //TODO have a dictionary of some common methods/packages and make all others @impure 
          println("! Call not pusca-compiled method " + s.owner.name + "." + s.name + "  (purity = " + purityOfType(resultType(s)) + ", resultType=" + resultType(s) + ")")
          purityOfType(resultType(s))
        case _ ⇒ AlwaysPure
      }
//      println("the purity of " + s + " is " + r)
      r
    }
    private def purityOfType(tpe: Type) = {
      val s = tpe.typeSymbol
      //TODO
      //AlwaysPure
      if (s.isTypeParameterOrSkolem) {
        //                println("purityOfType   " + tpe + "   " + s)
        //                println("@@@  parents = " + tpe.parents)
        //                println("@@@  owner = " + s.owner)
        //                println("@@@  underlying = " + tpe.underlying)
        //                println("@@@  dealias = " + tpe.dealias)
        ImpureDependingOn(Set(s.name.toString))
      } else if (hasAnnotation(tpe, Annotation.sideEffect)) AlwaysImpure
      else AlwaysPure
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
    def resolveTypeParams(a: Apply): Map[String, Type] = a.fun match {
      case TypeApply(fun, targs) ⇒
        val pl = fun.symbol.typeParams.map(_.name.toString).toList
        methodOwnerTypeParams(fun) ++ pl.zip(targs.map(_.tpe)).toMap
      case f ⇒ methodOwnerTypeParams(f)
    }
    def methodOwnerTypeParams(t: Tree): Map[String, Type] = {
      def typeParams(tpe: Type, lookingFor: Symbol): Map[String, Type] = {
        def tpsFor(o: Symbol, s: Symbol): Map[String, Type] = {
          s.typeParams.map(s ⇒ (s.name.toString, s.tpe.asSeenFrom(tpe, o))).toMap
        }
        lookingFor.ownerChain.view.filter(_.typeParams.nonEmpty).foldLeft(Map[String, Type]())((s, e) ⇒ tpsFor(e, e) ++ s)
      }
      t match {
        case s @ Select(o, _) ⇒ typeParams(o.tpe, s.symbol)
        case i: Ident         ⇒ typeParams(i.tpe, i.symbol)
        case _                ⇒ Map()
      }
    }

    /** checks whether an apply is pure given the list of with names of type parameters that are allowed to be impure. */
    def violatesPurity(a: Apply, allowedImpures: Set[String]): Boolean = {
//      if (a.fun.toString == "ip")
//        println("test")
//      println("processing " + a + " (" + a.fun + "    " + a.fun.symbol + ")")
//      println("@ purity of " + a + " is " + purityOf(a.fun.symbol))
      purityOf(a.fun.symbol) match {
        case AlwaysPure            ⇒ false
        case DeclaredPure          ⇒ false
        case AlwaysImpure          ⇒ true
        case ImpureDependingOn(di) ⇒ violates(di, allowedImpures, resolveTypeParams(a), a.pos, a.fun.symbol)
      }
    }
    def violatesPurity(s: Select, allowedImpures: Set[String]): Boolean = purityOf(s.symbol) match {
      case AlwaysPure            ⇒ false
      case DeclaredPure          ⇒ false
      case AlwaysImpure          ⇒ true
      case ImpureDependingOn(di) ⇒ violates(di, allowedImpures, methodOwnerTypeParams(s), s.pos, s.symbol)
    }
    private def violates(di: Set[String], allowedImpures: Set[String], tp: Map[String, Type], pos: Position, f: Symbol): Boolean = {
      def isAllowedImpure(tpe: Type) = tpe.typeSymbol.isTypeParameterOrSkolem && allowedImpures.contains(tpe.typeSymbol.name.toString)

      val tparams = tp.filter(e ⇒ di.contains(e._1))
      di.filterNot(tparams.contains).foreach { p ⇒ reporter.error(pos, "unresolved type parameter " + p + " on call to " + f.fullName) }
      //val ips = tparams.filter(e ⇒ hasSideEffect(e._2) && !isAllowedImpure(e._2))
      //TODO
      println("#tparams   " + tparams)
      //      tparams.foreach { tp ⇒
      //        val t = tp._2
      //        val s = t.typeSymbol
      //        println(" tp" + t)
      //        println("    da=" + t.dealias)
      //        println("    ud=" + t.underlying)
      //        println("    pa=" + t.parents)
      //        println("    ts=" + s)
      //        println("    oc=" + s.ownerChain)
      //      }

      //      val ips = tparams.filter(e ⇒ !isAllowedImpure(e._2))
      def sideEffectFreeParam(t: Type) = t.typeSymbol.isTypeParameterOrSkolem && hasAnnotation(t, Annotation.sideEffectFree)
      def sideEffectFreeValue(t: Type) = !t.typeSymbol.isTypeParameterOrSkolem && !hasAnnotation(t, Annotation.sideEffect)

      val ips = tparams.filterNot(e ⇒ isAllowedImpure(e._2) || sideEffectFreeParam(e._2) || sideEffectFreeValue(e._2))
      println("   ips = " + ips)
      ips.nonEmpty
    }
  }

  object PurityChecker {
    case class Error(pos: Position, msg: String) {
      def report = reporter.error(pos, msg)
    }

    import Purity._
    def apply(d: DefDef): List[Error] = apply(d.symbol, "method " + d.symbol.fullName, d.rhs)
    def apply(d: DefDef, asIfPure: Boolean = false): List[Error] = {
      if (asIfPure) handle(d.symbol, "method " + d.symbol.fullName, Set.empty)(d.rhs)
      else apply(d.symbol, "method " + d.symbol.fullName, d.rhs)
    }
    def apply(c: ClassDef): List[Error] = apply(c.symbol, "class " + c.symbol.fullName, c.impl)
    def apply(f: Function): List[Error] = apply(f.symbol, "function " + f.symbol.fullName, f.body)
    def apply(obj: Symbol, objName: String, content: Tree): List[Error] = {
      purityOf(obj) match {
        case AlwaysPure                 ⇒ handle(obj, objName, Set.empty)(content)
        case AlwaysImpure               ⇒ Nil
        case DeclaredPure               ⇒ Nil
        case ImpureDependingOn(impures) ⇒ handle(obj, objName, impures)(content)
      }
    }
    def isPure(f: Function): Boolean = isPure(f.symbol, f.body)
    def isPure(s: Symbol, t: Tree): Boolean = handle(s, "", Set.empty)(t).isEmpty

    private[this] def handle(obj: Symbol, objName: String, allowedImpures: Set[String])(t: Tree, soFar: List[Error] = Nil): List[Error] = t match {
      case a: Apply if violatesPurity(a, allowedImpures) ⇒
        val msg = a.fun.symbol match {
          case s if s.isSetter      ⇒ "write to non-local var " + s.name.toString.dropRight(4) + " inside the pure " + objName
          case s if s.isGetter      ⇒ "access to non-local var " + s.name + " inside the pure " + objName
          case s if s.isConstructor ⇒ "impure method call to " + s.owner.name + ".<init> inside the pure " + objName
          case s                    ⇒ "impure method call to " + s.name + " inside the pure " + objName
        }
        Error(a.pos, msg) :: soFar
      case a @ Apply(Select(o, _), args) ⇒
        (o :: args).foldLeft(soFar)((sf, e) ⇒ handle(obj, objName, allowedImpures)(e, sf))
      case a @ Apply(TypeApply(Select(o, _), _), args) ⇒
        (o :: args).foldLeft(soFar)((sf, e) ⇒ handle(obj, objName, allowedImpures)(e, sf))
      case a: Apply ⇒
        a.children.foldLeft(soFar)((sf, e) ⇒ handle(obj, objName, allowedImpures)(e, sf))

      case a @ Assign(s @ Select(_, _), _) if s.symbol.isModuleVar ⇒ //implementation detail of object (assigns a xxx$module var)
        soFar
      case a @ Assign(lhs, rhs) if (!lhs.symbol.ownerChain.contains(obj)) ⇒ // assign to var outside the scope of this method
        Error(a.pos, "write to non-local var " + lhs.symbol.name + " inside the pure " + objName) :: soFar

      case s: Select if s.symbol.isModuleVar ⇒ //implementation detail of object (reads a xxx$module var)
        soFar
      case s: Select if violatesPurity(s, allowedImpures) ⇒ //this probably only happens when called in the typer phase
        val msg = s.symbol match {
          case s if s.isGetter      ⇒ "access to non-local var " + s.name + " inside the pure " + objName
          case s if s.isConstructor ⇒ "impure method call to " + s.owner.name + ".<init> inside the pure " + objName
          case s                    ⇒ "impure method call to " + s.name + " inside the pure " + objName
        }
        Error(s.pos, msg) :: soFar
      case s: Select if s.symbol.isSetter ⇒ //assign to var via setter
        Error(s.pos, "write to non-local var " + s.name.toString.dropRight(4) + " inside the pure " + objName) :: soFar
      case s: Select if s.symbol.isMutable && !s.symbol.ownerChain.contains(obj) ⇒ // read of var outside the scope of this method (private[this])
        Error(s.pos, "access to non-local var " + s.name + " inside the pure " + objName) :: soFar
      case s: Select if s.symbol.isGetter && !s.symbol.isStable ⇒ // read of var via accessor
        Error(s.pos, "access to non-local var " + s.name + "inside the pure " + objName) :: soFar
      case Select(o, _) ⇒ handle(obj, objName, allowedImpures)(o, soFar)

      case i: Ident if i.symbol.isMutable && !i.symbol.ownerChain.contains(obj) ⇒ // read of var defined in an outer function
        Error(i.pos, "access to non-local var " + i.name + " inside the pure " + objName) :: soFar

      case d: DefDef   ⇒ soFar
      case c: ClassDef ⇒ soFar
      case f: Function ⇒ soFar
      case other       ⇒ other.children.foldLeft(soFar)((sf, e) ⇒ handle(obj, objName, allowedImpures)(e, sf))
    }
  }
}