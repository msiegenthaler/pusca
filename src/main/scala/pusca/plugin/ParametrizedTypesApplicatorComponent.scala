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
      case Apply(Select(Select(Ident(scalaName), symbolName), applyName), Literal(arg @ Constant(_)) :: Nil) if arg.tag == StringTag ⇒ Some(arg.stringValue)
      case _ ⇒ None
    }
  }

  private class Looker(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) ⇒
        val impureIf = d.symbol.annotations.find(_.atp.typeSymbol == Annotation.impureIf) match {
          case Some(AnnotationInfo(_, args, _)) ⇒ args.collect { case SymbolApply(arg) ⇒ arg }
          case None                             ⇒ Nil
        }
        //println("found defdef " + d.name + "  with impureIfs: " + impureIf) //TODO
        val appl = new Applicator(unit, impureIf.toSet)
        treeCopy.DefDef(d, mods, name, tparams, vparamss, tpt, appl.transform(d.rhs))
      //TODO ClassDef
      //TODO Function

      case other ⇒ super.transform(other)
    }

    private class Applicator(unit: CompilationUnit, impures: Set[String]) extends TypingTransformer(unit) {
      private object WithTypeVar {
        def unapply(t: Tree) = t.tpe match {
          case TypeRef(_, s, _) if s.isTypeParameterOrSkolem ⇒ Some(s.nameString)
          case _ ⇒ None
        }
      }
      override def transform(tree: Tree): Tree = tree match {
        case a @ ApplySideEffect(_) ⇒ a
        case a @ AddSideEffect(_)   ⇒ a
        case a: Apply ⇒ a match {
          case WithTypeVar(v) ⇒
            println("found var: " + v)
            if (impures.contains(v)) {
              println("is allowed to be impure")
              super.transform(a)
            } else {
              localTyper.typed(applySideEffect(a))
            }
          case _ ⇒ super.transform(a)
        }

        case d: DefDef   ⇒ Looker.this.transform(d)
        case c: ClassDef ⇒ Looker.this.transform(c)
        case f: Function ⇒ Looker.this.transform(f)

        case other       ⇒ super.transform(other)
      }
    }
  }
}