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
    def hasSideEffectFreeReturn(d: DefDef): Boolean = hasSideEffectFreeReturn(d.symbol)

    override def transform(tree: Tree): Tree = {
      val nt = tree match {
        case d: DefDef if d.symbol.hasAnnotation(Annotation.impureIfReturnType) ⇒
          if (hasSideEffectFreeReturn(d)) changePurityAnnotation(Annotation.pure)(d)
          d

        case d: DefDef ⇒
          d

        //TODO constructors (resp. ClassDef)

        case other ⇒ other
      }
      super.transform(nt)
    }
  }
  /*
  def newPhase(prev: Phase) = new NarrowPurity(prev)

  class NarrowPurity(prev: Phase) extends StdPhase(prev) {

    @tailrec private def returnTypeOf(methodType: Type): Type = methodType match {
      case PolyType(_, rt)      ⇒ returnTypeOf(rt)
      case MethodType(_, r)     ⇒ r
      case NullaryMethodType(r) ⇒ r
      case t                    ⇒ t
    }

    private def resolveOverridenType(implementor: Symbol)(baseMethod: Symbol) = implementor.tpe.memberInfo(baseMethod)

    def hasSideEffectFreeReturn(d: DefDef): Boolean = hasSideEffectFreeReturn(d.symbol)
    def hasSideEffectFreeReturn(method: Symbol): Boolean = {
      method.extendedOverriddenSymbols.view.map(resolveOverridenType(method.owner)).map(returnTypeOf).
        filter(SideEffectFreeType.isSideEffectFree).nonEmpty
    }

    override def apply(unit: CompilationUnit) {
      def look(t: Tree): Unit = {
        t match {
          case d: DefDef if d.symbol.hasAnnotation(Annotation.impureIfReturnType) && hasSideEffectFreeReturn(d) ⇒
            // TODO: write a component that replaces @impureIfReturnType with @pure for this case 
            println("must be pure: " + d.symbol.fullName)

          //TODO: should we switch to @pure if a method overriding a method annotated with @impureIfReturnType
          // and a non skolem/param return type? probably yes..
          // --> we probably need to do that anyway, not only on overriden methods..

          case _ ⇒ ()
        }
        t.children.foreach(look)
      }
      look(unit.body)
    }
  }
  */
}