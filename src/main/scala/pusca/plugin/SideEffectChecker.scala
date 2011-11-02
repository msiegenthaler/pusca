package pusca
package plugin

import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags._

abstract class SideEffectChecker extends PuscaDefinitions {
  val global: Global
  import global._

  object RemoveUnnecessaryApplySideEffect extends RemoveUnnecessaryApplySideEffectBase

  object checker extends AnnotationChecker {
    private def hasSideEffect(tpe: Type) = hasAnnotation(tpe, Annotation.sideEffect)
    private def withSideEffect(tpe: Type) = annotateWith(tpe, Annotation.sideEffect)

    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      //println("# annotations " + tpe1 + " conforms to " + tpe2 + ": " + (hasSideEffect(tpe2) || !hasSideEffect(tpe1)))
      hasSideEffect(tpe2) || !hasSideEffect(tpe1)
    }

    override def annotationsGlb(tp: Type, ts: List[Type]): Type = {
      //println("# Glb tp=" + tp + "    ts=" + ts)
      tp
    }

    override def annotationsLub(tp: Type, ts: List[Type]): Type = {
      //println("# Lub tp=" + tp + "    ts=" + ts)
      if (ts.find(hasAnnotation(_, Annotation.sideEffect)).isDefined && !hasAnnotation(tp, Annotation.sideEffect))
        annotateWith(tp, Annotation.sideEffect)
      else tp
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      //println("# addAnnotations type of tree=" + tree.getClass + "   tpe=" + tpe + "\n       tree=" + tree)
      tree match {
        case f @ Function(vparams, body) if !PureMethodChecker(f.symbol, "", RemoveUnnecessaryApplySideEffect.transform(body)).isEmpty ⇒
          //impure function, so annotate the return type
          tpe match {
            case r: TypeRef ⇒
              val (h, t :: Nil) = r.args.splitAt(r.args.size - 1)
              if (!hasAnnotation(t, Annotation.sideEffect)) {
                val nt = annotateWith(t, Annotation.sideEffect)
                val res = TypeRef(r.pre, r.sym, h ::: nt :: Nil)
                res
              } else tpe
            case other if (!hasAnnotation(other, Annotation.sideEffect)) ⇒
              annotateWith(tpe, Annotation.sideEffect)
            case _ ⇒ tpe
          }
        case _ ⇒ tpe
      }
    }

    override def canAdaptAnnotations(tree: Tree, mode: Int, pt: Type): Boolean = {
      //println("# canAdapt  tree=" + tree + "  mode=" + analyzer.modeString(mode) + "  pt=" + pt + "  class=" + tree.getClass)
      if ((mode & analyzer.EXPRmode) == 0 || (mode & analyzer.LHSmode) != 0) false
      else tree match {
        case a @ ApplySideEffect(_) ⇒ false
        case a @ MarkReturnValue(_, _) ⇒ true
        case a: Apply if hasAnnotation(a.tpe, Annotation.sideEffect) ⇒ true
        case i: Ident if hasAnnotation(i.tpe, Annotation.sideEffect) ⇒ true
        case s: Select if hasAnnotation(s.tpe, Annotation.sideEffect) ⇒ true
        case _ ⇒ false
      }
    }

    protected def applySideEffectFun = Select(Ident(puscaPackage.name.toTermName), "applySideEffect")
    protected def applySideEffect(v: Tree) = {
      val a = Apply(applySideEffectFun, v :: Nil)
      a.symbol.setFlag(SYNTHETIC)
      a.pos = v.pos
      a
    }
    protected def addSideEffectFun = Select(Ident(puscaPackage.name.toTermName), "addSideEffect")
    protected def addSideEffect(v: Tree) = {
      val a = Apply(addSideEffectFun, v :: Nil)
      a.symbol.setFlag(SYNTHETIC)
      a.pos = v.pos
      a
    }

    private[this] val recursive = new ThreadLocal[Boolean]
    def dontRecurse[A](rec: A)(f: ⇒ A): A = {
      if (recursive.get) rec
      else
        try {
          recursive.set(true)
          f
        } finally {
          recursive.set(false)
        }
    }
    override def adaptAnnotations(tree: Tree, mode: Int, pt: Type): Tree = {
      lazy val localTyper = analyzer.newTyper(analyzer.rootContext(currentRun.currentUnit, tree, false))
      def typed(a: Tree) = dontRecurse(tree)(localTyper.typed(a))

      //println("# adapt  tree=" + tree + "  mode=" + analyzer.modeString(mode) + "  pt=" + pt)
      tree match {
        // get rid of all markReturnValue calls
        case MarkReturnValue(ApplySideEffect(a), _) ⇒ //remove applySideEffect on the method's return path
          a
        case MarkReturnValue(a, true) if !hasAnnotation(a.tpe, Annotation.sideEffect) ⇒ //addSideEffect on return path
          typed(addSideEffect(a))
        case MarkReturnValue(a, _) ⇒
          a

        case a @ Apply(f @ Select(Super(This(_), _), n), _) if n == stringToTermName("<init>") ⇒
          a

        case a @ AddSideEffect(_) ⇒
          a

        case tree ⇒
          typed(applySideEffect(tree))
      }
    }

    override def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
      //println("# Bounds bounds=" + bounds + "   tparams" + tparams + "    " + targs)
      bounds.zip(targs).map { e ⇒
        val (bound, targ) = e
        if (hasSideEffect(targ)) TypeBounds(withSideEffect(bound.lo), withSideEffect(bound.hi))
        else bound
      }
    }
  }
}