package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags

/**
 * Checks for calls to impure methods and raises compilations errors if found.
 */
class PurityCheckerComponent(val global: Global) extends PluginComponent with PuscaDefinitions {
  import global._

  override val runsAfter = List("typer", "forbiddenSideEffectAssignment", "purityDeclarationConflictDetector", "uncurry")
  override val runsBefore = List("tailcalls")
  val phaseName = "purityChecker"
  def newPhase(prev: Phase) = new PurityChecker(prev)

  class PurityChecker(prev: Phase) extends StdPhase(prev) {
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

    /** Determines the declared purity of a method. Does not take the body of the method into account, whether the body conforms to the declaration is checked elsewhere. */
    def purityOf(s: Symbol): Purity = s match {
      case s if hasAnnotation(s, Annotation.pure) ⇒
        AlwaysPure
      case s if hasAnnotation(s, Annotation.impure) ⇒
        AlwaysImpure
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
      case _ ⇒
        AlwaysPure
    }
    private def resultType(t: MethodSymbol): Type = {
      def resTpe(t: Type): Type = t match {
        case PolyType(_, rt)      ⇒ resTpe(rt)
        case MethodType(_, r)     ⇒ r
        case NullaryMethodType(r) ⇒ r
      }
      resTpe(t.tpe)
    }
    sealed trait Purity
    case object AlwaysPure extends Purity
    case object AlwaysImpure extends Purity
    case class ImpureDependingOn(tparams: Set[String]) extends Purity

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

    override def apply(unit: CompilationUnit) {
      def look(t: Tree): Unit = t match {
        case d: DefDef   ⇒ checkPurity(d.symbol, d.rhs)
        case c: ClassDef ⇒ checkPurity(c.symbol, c.impl)

        //TODO check if we should run after uncurry and whether this works correctl
        case f: Function ⇒ checkPurity(f.symbol, f.body)

        case other ⇒
          other.children.foreach(look)
      }
      def checkPurity(s: Symbol, content: Tree): Unit = {
        purityOf(s) match {
          case AlwaysPure                 ⇒ inside(s, Set())(content)
          case AlwaysImpure               ⇒ look(content)
          case ImpureDependingOn(impures) ⇒ inside(s, impures)(content)
        }
      }
      def inside(method: Symbol, impures: Set[String])(t: Tree): Unit = t match {
        case a: Apply if violatesPurity(a, impures) ⇒
          reporter.error(a.pos, "impure method call inside the pure method '" + method.fullName + "'")

        case d: DefDef   ⇒ look(d)
        case c: ClassDef ⇒ look(c)
        case f: Function ⇒ look(f)

        case other       ⇒ other.children.foreach(inside(method, impures))

      }
      look(unit.body)
    }
  }
}