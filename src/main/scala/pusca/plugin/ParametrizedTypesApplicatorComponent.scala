package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.transform.TypingTransformers

/**
 * Adds applySideEffect to everything that results in a type parameter that might be @sideEffect.
 */
class ParametrizedTypesApplicatorComponent(val global: Global) extends PluginComponent with Transform with TypingTransformers with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("typer")
  val phaseName = "parametrizedTypesApplicator"
  def newTransformer(unit: CompilationUnit): Transformer = new Looker(unit)

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
  private object WithTypeVar {
    def unapply(t: Tree) = t.tpe match {
      case TypeRef(_, s, _) if s.isTypeParameterOrSkolem ⇒ Some(s.nameString)
      case _ ⇒ None
    }
  }

  //*** Start *****

  sealed trait Purity
  case object AlwaysPure extends Purity
  case object AlwaysImpure extends Purity
  case class ImpureDependingOn(tparams: Set[String]) extends Purity

  /** Determines the declared purity of a method. Does not take the body of the method into account, whether the body conforms to the declaration is checked elsewhere. */
  def methodPurity(s: Symbol): Purity = s match {
    case s: MethodSymbol if hasAnnotation(s, Annotation.pure) ⇒
      AlwaysPure
    case s: MethodSymbol if hasAnnotation(s, Annotation.impure) ⇒
      AlwaysImpure
    case s: MethodSymbol if hasAnnotation(s, Annotation.impureIf) ⇒
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
  def violatesPurity(a: Apply, allowedImpures: Set[String] = Set()): Boolean = methodPurity(a.fun.symbol) match {
    case AlwaysPure   ⇒ false
    case AlwaysImpure ⇒ true
    case ImpureDependingOn(di) ⇒
      def mayHaveSideEffect(tpe: Type) = hasSideEffect(tpe) || tpe.typeSymbol.isTypeParameterOrSkolem
      def isAllowedImpure(tpe: Type) = tpe.typeSymbol.isTypeParameterOrSkolem || allowedImpures.contains(tpe.typeSymbol.name.toString)

      val tparams = resolveTypeParams(a).filter(e ⇒ di.contains(e._1))
      di.filterNot(tparams.contains).foreach { p ⇒ reporter.error(a.pos, "unresolved type parameter " + p + " on call to " + a.fun.symbol.fullName) }
      val ips = tparams.filter(e ⇒ mayHaveSideEffect(e._2) && !isAllowedImpure(e._2))
      ips.foreach(i ⇒ println("@@  unallowed impure: " + i)) //TODO
      ips.nonEmpty
  }
  private class Looker(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) ⇒
        methodPurity(d.symbol) match {
          case AlwaysPure ⇒
            val appl = new Applicator(unit, d.symbol, Set())
            treeCopy.DefDef(d, mods, name, tparams, vparamss, tpt, appl.transform(d.rhs))
          case AlwaysImpure ⇒
            treeCopy.DefDef(d, mods, name, tparams, vparamss, tpt, transform(d.rhs))
          case ImpureDependingOn(impures) ⇒
            val appl = new Applicator(unit, d.symbol, impures)
            treeCopy.DefDef(d, mods, name, tparams, vparamss, tpt, appl.transform(d.rhs))
        }

      //TODO ClassDef
      //TODO Function (or shift after uncurry?)

      case other ⇒ super.transform(other)
    }

    private class Applicator(unit: CompilationUnit, method: Symbol, impures: Set[String]) extends TypingTransformer(unit) {
      override def transform(tree: Tree): Tree = tree match {
        case a @ ApplySideEffect(_) ⇒ a
        case a @ AddSideEffect(_)   ⇒ a
        case a: Apply if violatesPurity(a, impures) ⇒
          reporter.error(a.pos, "impure method call inside the pure method '" + method.fullName + "'")
          super.transform(a)

        case d: DefDef   ⇒ Looker.this.transform(d)
        case c: ClassDef ⇒ Looker.this.transform(c)
        case f: Function ⇒ Looker.this.transform(f)

        case other       ⇒ super.transform(other)
      }
    }
  }
}