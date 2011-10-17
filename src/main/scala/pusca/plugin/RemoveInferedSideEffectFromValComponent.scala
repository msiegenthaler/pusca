package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

class RemoveInferedSideEffectFromValComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("parser", "methodReturnTypeAnnotator")
  override val runsBefore = List("namer")
  val phaseName = "removeInferedSideEffectFromVal"
  def newTransformer(unit: CompilationUnit) = new RemoveInferedSideEffectFromVal

  class RemoveInferedSideEffectFromVal extends Transformer {
    protected def applySideEffectFun = Select(Select(Ident("pusca"), "package"), "applySideEffect")
    protected def applySideEffect(v: Tree) = Apply(applySideEffectFun, v :: Nil)

    override def transform(tree: Tree): Tree = tree match {
      //handle calls inside impure
      case v @ ValDef(_, _, TypeTree(), rhs) ⇒ //val/var without explicitly specified type
        v.copy(rhs = applySideEffect(transform(rhs)))

      case Apply(Apply(applySideEffectFun, _), args) ⇒ //method call to applySideEffect
        super.transform(tree)
      case Apply(fun, args) ⇒ // method calls
        Apply(transform(fun), args.map(transform).map(applySideEffect))

      case other ⇒ super.transform(other)
    }
  }

  //TODO remove unused calls in a later phase (after the typer)
}