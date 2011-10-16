package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

class RemoveSideEffectInsideImpureComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("parser", "methodReturnTypeAnnotator")
  override val runsBefore = List("namer")
  val phaseName = "removeSideEffectInsideImpure"
  def newTransformer(unit: CompilationUnit) = new MethodReturnTypeAnnotator

  class MethodReturnTypeAnnotator extends Transformer {
    protected def toPure(v: Tree) = Apply(Select(Select(Ident("pusca"), "Unsafe"), "withoutSideEffect"), v :: Nil)

    private var insideImpure: Boolean = false

    override def transform(tree: Tree): Tree = tree match {
      //keep track of insideImpure
      case d @ DefDef(m, n, t, v, tpt, rhs) ⇒
        val oi = insideImpure
        if (hasAnnotation(d.tpt, Annotation.sideEffect)) {
          insideImpure = true
          val nrhs = transform(rhs)
          insideImpure = oi
          d.copy(rhs = nrhs)
        } else super.transform(d)

      //handle calls inside impure
      case v @ ValDef(_, _, tpt, a: Apply) if insideImpure ⇒
        val na = toPure(transform(a))
        v.copy(rhs = na, tpt = deannotate(tpt, Annotation.sideEffect))
      case v @ ValDef(_, _, tpt, i: Ident) if insideImpure ⇒
        val ni = toPure(transform(i))
        v.copy(rhs = ni, tpt = deannotate(tpt, Annotation.sideEffect))
      case a @ Apply(fun, args) if insideImpure ⇒
        val nargs = args.map(a ⇒ toPure(transform(a)))
        Apply(fun, nargs)

      case other ⇒ super.transform(other)
    }
  }
  
  //TODO add a phase to remove this inserted statements. At least the impure ones
}