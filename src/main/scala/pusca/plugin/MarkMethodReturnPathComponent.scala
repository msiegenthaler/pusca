package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/** Marks all statements on the 'path' of a method's return value */
class MarkMethodReturnPathComponent(val global: Global) extends PluginComponent with Transform with PuscaDefinitions with ParserStageSupport {
  import global._

  val runsAfter = List("parser", "methodReturnTypeAnnotator")
  override val runsBefore = List("namer")
  val phaseName = "markMethodReturnPath"
  def newTransformer(unit: CompilationUnit) = new MarkMethodReturnPath

  class MarkMethodReturnPath extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case d: DefDef ⇒
        val needToAddSideEffect = hasAnnotation(d, Annotation.impure) && !hasAnnotation(d.tpt, Annotation.sideEffect)
        val nrhs = transformReturn(d.rhs, needToAddSideEffect)
        treeCopy.DefDef(d, d.mods, d.name, d.tparams, d.vparamss, d.tpt, nrhs)
      case other ⇒ super.transform(other)
    }

    protected def markerFun = Select(Ident("pusca"), "markReturnValue")
    protected def markerSideEffectFun = Select(Ident("pusca"), "markReturnValueWithSideEffect")
    protected def mark(v: Tree, addSideEffect: Boolean) = {
      val f = if (addSideEffect) markerSideEffectFun else markerFun
      val a = Apply(f, v :: Nil)
      a.pos = v.pos
      a
    }
    def transformReturn(tree: Tree, addSideEffect: Boolean): Tree = {
      tree match {
        case a: Apply  ⇒ mark(a, addSideEffect)
        case i: Ident  ⇒ mark(i, addSideEffect)
        case s: Select ⇒ mark(s, addSideEffect)

        case b @ Block(stmts, expr) ⇒
          val ne = transformReturn(expr, addSideEffect)
          treeCopy.Block(b, stmts, ne)
        //TODO if, match, try, ...

        case other ⇒ super.transform(other)
      }
    }
  }
}
