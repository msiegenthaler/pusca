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

  //    private var inRelevant = false
  //    private def isRelevant: Boolean = inRelevant
  //    private def relevant[A](f: ⇒ A): A = {
  //      inRelevant = true
  //      val r = f
  //      inRelevant = false
  //      r
  //    }
  //
  //    override def transform(tree: Tree): Tree = tree match {
  //      case d: DefDef ⇒
  //        val nrhs = relevant(transform(d.rhs))
  //        d.copy(rhs = nrhs)
  //      case c: ClassDef ⇒
  //        c.copy(impl = transform(c.impl).asInstanceOf[Template])
  //      case t: Template ⇒
  //        val nb = relevant(t.body.map(transform))
  //        t.copy(body = nb)
  //      case b: Block ⇒ relevant {
  //        b.copy(expr = transform(b.expr), stats = b.stats.map(transform))
  //      }
  //
  //      //val/var without explicitly specified type
  //      case v @ ValDef(_, _, TypeTree(), ApplySideEffectFun(_)) if isRelevant ⇒
  //        super.transform(v)
  //      case v @ ValDef(_, _, TypeTree(), rhs) if isRelevant ⇒
  //        v.copy(rhs = applySideEffect(transform(rhs)))
  //
  //      //method calls
  //      case a @ ApplySideEffectFun(_) ⇒ //method call to applySideEffect
  //        super.transform(a)
  //      case Apply(fun, args) if isRelevant ⇒ // other method calls
  //        applySideEffect(Apply(transform(fun), args.map(transform).map(applySideEffect)))
  //
  //      case other ⇒
  //        super.transform(other)
  //    }
  //  }
}