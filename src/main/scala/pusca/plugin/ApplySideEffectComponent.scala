package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/**
 * Inserts calls to applySideEffect (converts A @sideEffect to A) on all method calls.
 */
class ApplySideEffectComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("parser", "methodReturnTypeAnnotator")
  override val runsBefore = List("namer")
  val phaseName = "applySideEffect"
  def newTransformer(unit: CompilationUnit) = new ApplySideEffect

  class ApplySideEffect extends Transformer {
    protected def applySideEffectFun = Select(Ident("pusca"), "applySideEffect")
    protected def applySideEffect(v: Tree) = {
      val a = Apply(applySideEffectFun, v :: Nil)
      a.pos = v.pos
      a
    }

    protected object ApplySideEffectFun {
      def unapply(t: Tree) = t match {
        case Apply(Select(Select(Ident(p), pko), mn), arg :: Nil) if p == stringToTermName("pusca") && pko == stringToTermName("package") && mn == stringToTermName("applySideEffect") ⇒
          Some(arg)
        case Apply(Select(Ident(p), mn), arg :: Nil) if p == stringToTermName("pusca") && mn == stringToTermName("applySideEffect") ⇒
          Some(arg)
        case _ ⇒ None
      }
    }

    override def transform(tree: Tree): Tree = findStatementBlock(tree)

    protected def findStatementBlock(t: Tree): Tree = t match {
      case t: Template ⇒
        val body = t.body.map(handleStatementBlock)
        treeCopy.Template(t, t.parents, t.self, body)
      case d: DefDef ⇒
        val rhs = handleStatementBlock(d.rhs)
        treeCopy.DefDef(d, d.mods, d.name, d.tparams, d.vparamss, d.tpt, rhs)
      case v: ValDef ⇒
        val rhs = handleStatementBlock(v.rhs)
        treeCopy.ValDef(v, v.mods, v.name, v.tpt, rhs)
      case b: Block ⇒
        val expr = handleStatementBlock(b.expr)
        val stats = b.stats.map(handleStatementBlock)
        treeCopy.Block(b, stats, expr)

      case other ⇒ super.transform(other)
    }

    protected def handleStatementBlock(t: Tree): Tree = t match {
      case a @ ApplySideEffectFun(args) ⇒ //method call to applySideEffect
        a
      case a @ Apply(Select(Super(_, _), n), _) if n == stringToTermName("<init>") ⇒ // don't do anything with super.<init>
        a
      case a @ Apply(fun, args) ⇒ // other method calls
        val nargs = args.map(handleStatementBlock)
        applySideEffect(treeCopy.Apply(a, fun, nargs))

      case i: Ident ⇒ //only the outermost
        applySideEffect(i)

      case other ⇒ findStatementBlock(other)
    }
  }
}