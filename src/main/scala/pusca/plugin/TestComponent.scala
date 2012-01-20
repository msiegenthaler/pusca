package pusca.plugin

import scala.annotation.tailrec
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags

/**
 * TODO delete
 */
class TestComponent(val global: Global) extends PluginComponent with PuscaDefinitions {
  import global._

  override val runsAfter = List("typer", "uncurry", "narrowPurity")
  override val runsBefore = List("tailcalls")
  val phaseName = "justatest"
  def newPhase(prev: Phase) = new Tester(prev)

  class Tester(prev: Phase) extends StdPhase(prev) {

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
}