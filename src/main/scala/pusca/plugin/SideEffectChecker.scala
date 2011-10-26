package pusca
package plugin

import scala.tools.nsc.Global

abstract class SideEffectChecker extends PuscaDefinitions {
  val global: Global
  import global._

  object RemoveUnnecessaryApplySideEffect extends RemoveUnnecessaryApplySideEffectBase

  object checker extends AnnotationChecker {
    private def hasSideEffect(tpe: Type) = hasAnnotation(tpe, Annotation.sideEffect)
    private def withSideEffect(tpe: Type) = annotateWith(tpe, Annotation.sideEffect)

    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      hasSideEffect(tpe2) || !hasSideEffect(tpe1)
    }

    override def annotationsGlb(tp: Type, ts: List[Type]): Type = {
      //println("# Glb tp=" + tp + "    ts=" + ts)
      tp
    }

    override def annotationsLub(tp: Type, ts: List[Type]): Type = {
      //println("# Lub tp=" + tp + "    ts=" + ts)
      tp
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      //println("# addAnnotations type of tree=" + tree.getClass + "   tpe=" + tpe + "\n       tree=" + tree)
      //TODO still needed?
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
      //println("# canAdapt  tree=" + tree + "  mode=" + mode + "  pt=" + pt)
      tree match {
        case ApplySideEffect(_) ⇒ false
        case a: Apply if hasAnnotation(a.tpe, Annotation.sideEffect) ⇒ true
        case i: Ident if (hasAnnotation(i.tpe, Annotation.sideEffect)) ⇒ true
        case s: Select if (hasAnnotation(s.tpe, Annotation.sideEffect)) ⇒ true
        case _ ⇒ false
      }
    }

    protected def applySideEffectFun = Select(Ident(puscaPackage.name.toTermName), "applySideEffect")
    protected def applySideEffect(v: Tree) = {
      val a = Apply(applySideEffectFun, v :: Nil)
      a.pos = v.pos
      a
    }

    private[this] val recursive = new ThreadLocal[Boolean]
    def recursiveSection[A](rec: A)(f: ⇒ A): A = {
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
      println("# adapt  tree=" + tree + "  mode=" + analyzer.modeString(mode) + "  pt=" + pt)
      recursiveSection(tree) {
        val localTyper = analyzer.newTyper(analyzer.rootContext(currentRun.currentUnit, tree, false))
        val nt = applySideEffect(tree)

        localTyper.typed(nt)
      }
    }

    override def adaptBoundsToAnnotations(bounds: List[TypeBounds],
                                          tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
      //println("# Bounds bounds=" + bounds + "   tparams" + tparams + "    " + targs)
      bounds.zip(targs).map { e ⇒
        val (bound, targ) = e
        if (hasSideEffect(targ)) TypeBounds(withSideEffect(bound.lo), withSideEffect(bound.hi))
        else bound
      }
    }
  }
}