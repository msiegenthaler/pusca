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
      case d: DefDef if !hasAnnotation(d, Annotation.pure) ⇒
        val needToAddSideEffect = hasAnnotation(d, Annotation.impure) && !hasAnnotation(d.tpt, Annotation.sideEffect)
        val nrhs = transformReturn(d.rhs, needToAddSideEffect)
        treeCopy.DefDef(d, d.mods, d.name, d.tparams, d.vparamss, d.tpt, nrhs)
      case f: Function ⇒
        val nb = transformReturn(f.body, false)
        treeCopy.Function(f, f.vparams, nb)
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
    def transformReturn(tree: Tree, se: Boolean): Tree = {
      tree match {
        case a: Apply   ⇒ mark(a, se)
        case i: Ident   ⇒ mark(i, se)
        case s: Select  ⇒ mark(s, se)
        case l: Literal ⇒ mark(l, se)

        case b @ Block(stmts, expr) ⇒
          val ne = transformReturn(expr, se)
          treeCopy.Block(b, stmts, ne)
        case i @ If(c, t, e) ⇒
          treeCopy.If(i, c, transformReturn(t, se), transformReturn(e, se))
        case m @ Match(sel, cases) ⇒
          val nc = cases.map(transformReturn(_, se).asInstanceOf[CaseDef])
          treeCopy.Match(m, sel, nc)
        case t @ Try(b, cs, f) ⇒
          val ncs = cs.map(transformReturn(_, se).asInstanceOf[CaseDef])
          if (f.isEmpty) treeCopy.Try(t, transformReturn(b, se), ncs, f)
          else treeCopy.Try(t, b, ncs, transformReturn(f, se))
        case c @ CaseDef(p, g, expr) ⇒
          treeCopy.CaseDef(c, p, g, transformReturn(expr, se))

        case other ⇒ super.transform(other)
      }
    }
  }
}
