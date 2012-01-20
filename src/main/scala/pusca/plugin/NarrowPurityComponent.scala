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
    def extendsSefReturn(method: Symbol) = method.extendedOverriddenSymbols.view.map(resolveOverridenType(method.owner)).map(returnTypeOf).
      filter(SideEffectFreeType.isSideEffectFree).nonEmpty
    def concreteReturn(method: Symbol) = !returnTypeOf(method.tpe).typeSymbol.isTypeParameterOrSkolem

    override def transform(tree: Tree): Tree = {
      tree match {
        case d: DefDef ⇒ d match {
          //if a method returns a concrete value (= not a skolem or type param) it is pure if not specified otherwise
          case PurityDecl.impureIfReturnType(s, _) if concreteReturn(s) || extendsSefReturn(s) ⇒
            changePurityAnnotation(PurityDecl.pure.annotation)(d)

          // if at least one of the super-defs has the @sideEffectFree on the return type then this def must be pure
          case PurityDecl.impureIfReturnType(s, _) if extendsSefReturn(s) ⇒
            changePurityAnnotation(PurityDecl.pure.annotation)(d)

          // if at least one of the super-defs has the @sideEffectFree on the return type then this def cannot be impure
          case PurityDecl.impure(s) if extendsSefReturn(s) ⇒
            reporter.error(d.pos, "The method " + s.name + " cannot be impure, its return type is parametrized to be side-effect free")

          //TODO handle impureIf

          case _ ⇒ () //don't change anything otherwise
        }

        //TODO constructors (resp. ClassDef)

        case other ⇒ ()
      }
      super.transform(tree)
    }
  }
}