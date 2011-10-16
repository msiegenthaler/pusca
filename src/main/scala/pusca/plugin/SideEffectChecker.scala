package pusca
package plugin

import scala.tools.nsc.Global

abstract class SideEffectChecker {
  val global: Global
  import global._

  object checker extends AnnotationChecker {
    private val sideEffectAnnotationType = definitions.getClass("pusca.sideEffect")
    private def hasSideEffect(tpe: Type) = tpe.annotations.find(_.atp.typeSymbol == sideEffectAnnotationType).isDefined
    private def sideEffectAnnotation = AnnotationInfo(sideEffectAnnotationType.tpe, Nil, Nil)
    private def withSideEffect(tpe: Type) = if (hasSideEffect(tpe)) tpe else tpe.withAnnotations(sideEffectAnnotation :: tpe.annotations)

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