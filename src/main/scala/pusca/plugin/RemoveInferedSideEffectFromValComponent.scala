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
    protected def applySideEffectFun = Select(Ident("pusca"), "applySideEffect")
    protected def applySideEffect(v: Tree) = Apply(applySideEffectFun, v :: Nil)

    protected object ApplySideEffectFun {
      def unapply(t: Tree) = t match {
        case Apply(Select(Select(Ident(p), pko), mn), arg :: Nil) if p == stringToTermName("pusca") && pko == stringToTermName("package") && mn == stringToTermName("applySideEffect") ⇒
          Some(arg)
        case Apply(Select(Ident(p), mn), arg :: Nil) if p == stringToTermName("pusca") && mn == stringToTermName("applySideEffect") ⇒
          Some(arg)
        case _ ⇒ None
      }
    }

    override def transform(tree: Tree): Tree = tree match {
      //val/var without explicitly specified type
      case v @ ValDef(_, _, TypeTree(), ApplySideEffectFun(_)) ⇒
        super.transform(v)
      case v @ ValDef(_, _, TypeTree(), rhs) ⇒
        v.copy(rhs = applySideEffect(transform(rhs)))

      //method calls
      case a @ ApplySideEffectFun(_) ⇒ //method call to applySideEffect
        super.transform(a)
      case Apply(fun, args) ⇒ // other method calls
        Apply(transform(fun), args.map(transform).map(applySideEffect))

      case other ⇒ super.transform(other)
    }
  }
}