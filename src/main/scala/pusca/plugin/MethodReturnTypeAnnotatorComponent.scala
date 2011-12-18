package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/**
 * Changes the return type of methods annotated with @impure by adding @sideEffect and adds @impureIfReturnType to unannotated
 * methods.
 */
class MethodReturnTypeAnnotatorComponent(val global: Global) extends PluginComponent with Transform with ParserStageSupport {
  import global._

  val runsAfter = List("parser")
  override val runsBefore = List("namer")
  val phaseName = "methodReturnTypeAnnotator"
  def newTransformer(unit: CompilationUnit) = new MethodReturnTypeAnnotator

  class MethodReturnTypeAnnotator extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Constructor(d) ⇒
        //TODO Handle impure classes
        super.transform(d)

      case d: DefDef if !hasPuscaMethodAnnotation(d) ⇒
        //annotate all methods without an annotation with @impureIfReturnType
        val nmods = d.mods.withAnnotations(makeAnnotation(Annotation.impureIfReturnType) :: d.mods.annotations)
        val ndef = treeCopy.DefDef(d, nmods, d.name, d.tparams, d.vparamss, d.tpt, d.rhs)
        transform(ndef) //process again

      //return type is inferred
      case d @ DefDef(_, _, _, _, TypeTree(), rhs) ⇒ //no return type declared, so mark the return path
        val ndef = {d match {
          case d: DefDef if hasAnnotation(d, Annotation.pure) ⇒ Some(MarkSideEffectFree)
          case d: DefDef if hasAnnotation(d, Annotation.impure) ⇒ Some(MarkSideEffect)
          case _ ⇒ None
        }}.map { m =>
	        val nrhs = MarkerFun(m)(rhs)
	        treeCopy.DefDef(d, d.mods, d.name, d.tparams, d.vparamss, TypeTree(), nrhs)
        }.getOrElse(d)
        super.transform(ndef)

      //annotate the return type with @sideEffect or @sideEffectFree
      case d: DefDef if hasAnnotation(d, Annotation.impureIf) ⇒
        //TODO transform to impureIfReturnType, if the names match
        super.transform(d)
      case d: DefDef if hasAnnotation(d, Annotation.impureIfReturnType) ⇒
        //TODO
        super.transform(d)
      case d: DefDef if hasAnnotation(d, Annotation.impure) && !hasAnnotation(d.tpt, Annotation.sideEffect) ⇒
        val ntpt = annotate(d.tpt, Annotation.sideEffect)
        val ndef = treeCopy.DefDef(d, d.mods, d.name, d.tparams, d.vparamss, ntpt, d.rhs)
        super.transform(ndef)
      case d: DefDef if hasAnnotation(d, Annotation.pure) && !hasAnnotation(d.tpt, Annotation.sideEffectFree) ⇒
        val ntpt = annotate(d.tpt, Annotation.sideEffectFree)
        val nrhs = MarkerFun(MarkSideEffectFree)(d.rhs) //may cause impure to be 'casted' to pure, but the PurityChecker will find out 
        val ndef = treeCopy.DefDef(d, d.mods, d.name, d.tparams, d.vparamss, ntpt, nrhs)
        super.transform(ndef)

      //annotate the return values of anon functions with infere
      case f @ Function(vp, body) ⇒
        val nb = MarkerFun(MarkInfere)(f.body)
        val nf = treeCopy.Function(f, vp, nb)
        super.transform(nf)

      //TODO also handle conflicts? @pure def a: Int @sideEffect

      //TODO check for the annotations in different places than type parameters and method returns and error them.

      case other ⇒
        super.transform(tree)
    }

    object MarkerFun {
      private[this] def markFun(name: String) = Select(Select(Ident("pusca"), "Internal"), name)
      private def markInfere = markFun("markInfere")
      private def markSideEffect = markFun("markSideEffect")
      private def markSideEffectFree = markFun("markSideEffectFree")
      private def markFunFor(mark: Mark) = mark match {
        case MarkInfere         ⇒ markInfere
        case MarkSideEffect     ⇒ markSideEffect
        case MarkSideEffectFree ⇒ markSideEffectFree
      }

      def apply(mark: Mark)(fun: Tree): Tree = {
        val a = Apply(markFunFor(mark), fun :: Nil)
        a.pos = fun.pos
        a
      }
    }
    /*
    def transformReturn(tree: Tree, mark: Mark): Tree = {
      tree match {
        case a: Apply   ⇒ mark(a, m)
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
          treeCopy.Try(t, transformReturn(b, se), ncs, f)
        case c @ CaseDef(p, g, expr) ⇒
          treeCopy.CaseDef(c, p, g, transformReturn(expr, se))

        case other ⇒ super.transform(other)
      }
    }
    */
  }
}