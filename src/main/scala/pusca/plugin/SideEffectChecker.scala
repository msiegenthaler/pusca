package pusca
package plugin

import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags._

abstract class SideEffectChecker extends PuscaDefinitions {
  val global: Global
  import global._
  import definitions._

  object checker extends AnnotationChecker {
    //TODO move to PuscaDefinition
    /** A side-effect free type */
    object SideEffectFreeType {
      def unapply(t: Type) = {
        if (t.dealias.hasAnnotation(Annotation.sideEffectFree)) Some(t)
        else None
      }
    }
    /** A type with side effect */
    object SideEffectType {
      def unapply(t: Type) = {
        if (t.dealias.hasAnnotation(Annotation.sideEffect)) Some(t)
        else None
      }
    }
    /** A type with neither @sideEffect and @sideEffectFree */
    object UnspecifiedSideEffectType {
      def unapply(t: Type) = {
        if (!t.dealias.hasAnnotation(Annotation.sideEffect) && !t.dealias.hasAnnotation(Annotation.sideEffectFree)) Some(t)
        else None
      }
    }
    /** A type on the return path, that has been marked by MethodReturnTypeAnnotatorComponent */
    object MarkReturnType {
      def unapply(t: Type) = t match {
        case t if t.hasAnnotation(Annotation.returnedInfere) ⇒ Some(MarkInfere)
        case t if t.hasAnnotation(Annotation.returnedSideEffect) ⇒ Some(MarkSideEffect)
        case t if t.hasAnnotation(Annotation.returnedSideEffectFree) ⇒ Some(MarkSideEffectFree)
        case _ ⇒ None
      }
    }

    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      // Example:
      //   val a: <tpe1>
      //   val b: <tpe2> = a
      println("# annotations " + tpe1 + " conforms to " + tpe2)
      val r = tpe2 match {
        case SideEffectFreeType(_) ⇒ tpe1 match {
          case SideEffectFreeType(_)              ⇒ true
          case MarkReturnType(MarkSideEffectFree) ⇒ true
          case MarkReturnType(MarkInfere)         ⇒ true
          case t if t.typeSymbol == NothingClass  ⇒ true //'Nothing' is sideEffectFree, because otherwise it's not ground
          case _                                  ⇒ false
        }
        case _ ⇒ true
      }
      println("      conform = " + r)
      r
    }

    override def annotationsGlb(tp: Type, ts: List[Type]): Type = {
      //println("# Glb tp=" + tp + "    ts=" + ts)
      //TODO implement
      tp
    }

    override def annotationsLub(tp: Type, ts: List[Type]): Type = {
      //println("# Lub tp=" + tp + "    ts=" + ts)
      //TODO implement
      tp
    }

    override def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
      //println("# Bounds bounds=" + bounds + "   tparams" + tparams + "    " + targs)
      //TODO implement
      bounds
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      //println("# addAnnotations type of tree=" + tree.getClass + "   tpe=" + tpe + "\n       tree=" + tree)
      tree match {
        case f @ Function(vparams, body) if PurityChecker(f).nonEmpty ⇒
          //impure function, so annotate the return type with @sideEffect
          tpe match {
            case r: TypeRef ⇒
              val param :: result :: Nil = r.args
              result match {
                case SideEffectType(t) ⇒ tpe
                case SideEffectFreeType(t) ⇒
                  reporter.error(tree.pos, "Impure function may not return a type annotated with @sideEffectFree: " + t)
                  tpe
                case t ⇒
                  val nt = removeAnnotation(annotateWith(t, Annotation.sideEffect), Annotation.returnedInfere)
                  TypeRef(r.pre, r.sym, param :: nt :: Nil)
              }
            case t ⇒
              reporter.warning(tree.pos, "Unexpected function type: " + t)
              tpe
          }
        case Function(vparams, body) ⇒
          //pure function, so annotate the return type with @sideEffectFree
          tpe match {
            case r: TypeRef ⇒
              val param :: result :: Nil = r.args
              result match {
                case SideEffectFreeType(t) ⇒ tpe
                case SideEffectType(t)     ⇒ tpe
                case t ⇒
                  val nt = removeAnnotation(annotateWith(t, Annotation.sideEffectFree), Annotation.returnedInfere)
                  TypeRef(r.pre, r.sym, param :: nt :: Nil)
              }
            case t ⇒
              reporter.warning(tree.pos, "Unexpected function type: " + t)
              tpe
          }
        case _ ⇒ tpe
      }
    }

    override def canAdaptAnnotations(tree: Tree, mode: Int, pt: Type): Boolean = {
      println("# canAdapt  tree=" + tree + "  mode=" + analyzer.modeString(mode) + "  pt=" + pt + "  class=" + tree.getClass)
      false
    }

    override def adaptAnnotations(tree: Tree, mode: Int, pt: Type): Tree = {
      //println("# adapt  tree=" + tree + "  mode=" + analyzer.modeString(mode) + "  pt=" + pt)
      tree
    }
  }
}