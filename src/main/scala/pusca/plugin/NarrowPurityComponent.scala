package pusca.plugin

import scala.annotation.tailrec
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.transform.Transform

/**
 * Narrows down the purity of the @impureIfReturnType down to @pure if the compiler can prove it to be pure.
 */
class NarrowPurityComponent(val global: Global) extends PluginComponent with PuscaDefinitions with Transform {
  import global._
  import Utils._

  override val runsAfter = List("typer")
  override val runsBefore = List("uncurry", "tailcalls")
  val phaseName = "narrowPurity"

  def newTransformer(unit: CompilationUnit) = new NarrowPurity

  class NarrowPurity extends Transformer {
    private def resolveOverridenType(implementor: Symbol)(baseMethod: Symbol) = implementor.tpe.memberInfo(baseMethod)
    def hasSideEffectFreeReturn(method: Symbol): Boolean = {
      def extendsWithSefParam = method.extendedOverriddenSymbols.view.map(resolveOverridenType(method.owner)).map(returnTypeOf).
        filter(SideEffectFreeType.isSideEffectFree).nonEmpty
      def concrete = !returnTypeOf(method.tpe).typeSymbol.isTypeParameterOrSkolem
      concrete || extendsWithSefParam
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case d: DefDef ⇒ d match {
          case PurityDecl.impureIfReturnType(s, _) ⇒
            if (hasSideEffectFreeReturn(s)) changePurityAnnotation(PurityDecl.pure.annotation)(d)
          case _ ⇒ ()
        }
        //TODO constructors (resp. ClassDef)

        case other ⇒ ()
      }
      super.transform(tree)
    }
  }
}