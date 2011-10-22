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
      //      println("# conform tp1=" + tpe1 + "    tp2=" + tpe2)
      hasSideEffect(tpe2) || !hasSideEffect(tpe1)
    }

    override def annotationsGlb(tp: Type, ts: List[Type]): Type = {
      //      println("# Glb tp=" + tp + "    ts=" + ts)
      tp
    }

    override def annotationsLub(tp: Type, ts: List[Type]): Type = {
      //      println("# Lub tp=" + tp + "    ts=" + ts)
      tp
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type = tree match {
      case f @ Function(vparams, body) ⇒
        val t = RemoveUnnecessaryApplySideEffect.transform(body)
        if (!PureMethodChecker(f.symbol, body).isEmpty) {
          //impure function, so annotate the return type
          tpe match {
            case r: TypeRef ⇒
              val (h, t :: Nil) = r.args.splitAt(r.args.size - 1)
              val nt = annotateWith(t, Annotation.sideEffect)
              TypeRef(r.pre, r.sym, h ::: nt :: Nil)
            case _ ⇒ annotateWith(tpe, Annotation.sideEffect)
          }
        } else tpe
      case _ ⇒ tpe
    }

    private def handleStatements(t: Tree) = {
    }

    override def canAdaptAnnotations(tree: Tree, mode: Int, pt: Type): Boolean = {
      //      println("# canAdapt  tree=" + tree + "  mode=" + mode + "  pt=" + pt)
      false
    }

    override def adaptAnnotations(tree: Tree, mode: Int, pt: Type): Tree = {
      //      println("# adapt  tree=" + tree + "  mode=" + mode + "  pt=" + pt)
      tree
    }

    override def adaptBoundsToAnnotations(bounds: List[TypeBounds],
                                          tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
      //      println("# Bounds bounds=" + bounds + "   tparams" + tparams + "    " + targs)
      bounds.zip(targs).map { e ⇒
        val (bound, targ) = e
        if (hasSideEffect(targ)) TypeBounds(withSideEffect(bound.lo), withSideEffect(bound.hi))
        else bound
      }
    }
  }
}