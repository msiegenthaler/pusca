package pusca
package plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.transform.TypingTransformers
import Internal._

/** Replaces calls to pusca.Internal.purityOf(f) with the purity of f. */
class PurityOfReplacerComponent(val global: Global) extends PluginComponent with PuscaDefinitions with Transform with TypingTransformers {
  import global._

  override val runsAfter = List("typer")
  override val runsBefore = List("tailcalls", "uncurry")
  val phaseName = "purityOfReplacer"
  def newTransformer(unit: CompilationUnit) = new PurityOfReplacer(unit)

  class PurityOfReplacer(unit: CompilationUnit) extends TypingTransformer(unit) {
    /** Call to pusca.Internal.purityOf */
    object PurityOfCall {
      def unapply(t: Tree) = {
        if (t.symbol == PuscaMethods.purityOfMethod) {
          t match {
            case Apply(_, f :: Nil) ⇒ Some(f)
            case _                  ⇒ throw new IllegalArgumentException("weird call to pusca.Internal.purityOf")
          }
        } else None
      }
    }

    private def puscaInternalTree = Select(Ident("pusca"), "Internal")
    def makePurityTree(purity: Purity, pos: Position) = {
      val t = purity match {
        case AlwaysPure   ⇒ Select(puscaInternalTree, "AlwaysPure")
        case AlwaysImpure ⇒ Select(puscaInternalTree, "AlwaysImpure")
        //TODO
      }
      t.pos = pos
      localTyper.typed(t)
    }

    override def transform(tree: Tree): Tree = tree match {
      case PurityOfCall(b) ⇒ makePurityTree(TreePurity.of(b), tree.pos)
      case other           ⇒ super.transform(other)
    }
  }
}