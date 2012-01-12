package pusca.plugin.old

import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags._

abstract class SideEffectChecker extends PuscaDefinitions {
  val global: Global
  import global._
  import definitions._

  object checker extends AnnotationChecker {
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      // Example:
      //   val a: <tpe1>
      //   val b: <tpe2> = a
      println("# annotations " + tpe1 + " conforms to " + tpe2)
      val r = tpe2 match {
        case SideEffectFreeType(_) ⇒ tpe1 match {
          case SideEffectFreeType(_)             ⇒ true
          case MarkInfereReturnType(_)           ⇒ true
          case t if t.typeSymbol == NothingClass ⇒ true //'Nothing' is sideEffectFree, because otherwise it's not a subclass of everything
          case _                                 ⇒ false
        }
        case _ ⇒ true
      }
      println("      conform = " + r)
      r
    }

    override def annotationsGlb(tp: Type, ts: List[Type]): Type = {
      println("# Glb tp=" + tp + "    ts=" + ts)
      //TODO implement
      tp
    }

    override def annotationsLub(tp: Type, ts: List[Type]): Type = {
      println("# Lub tp=" + tp + "    ts=" + ts)
      //TODO implement
      tp
    }

    override def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
      println("# Bounds bounds=" + bounds + "   tparams" + tparams + "    " + targs)
      //TODO implement
      bounds
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      //println("# addAnnotations type of tree=" + tree.getClass + "   tpe=" + tpe + "\n       tree=" + tree)
      tree match {
        case f @ Function(vparams, body) if PurityChecker.isPure(f) ⇒
          println("Pure function " + f)
          //pure function, so annotate the return type with @sideEffectFree
          tpe match {
            case TypeRef(pre, sym, param :: result :: Nil) ⇒
              result match {
                case SideEffectFreeType(t) ⇒ tpe
                case SideEffectType(t)     ⇒ tpe
                case t ⇒
                  val nt = removeAnnotation(annotateWith(t, Annotation.sideEffectFree), Annotation.returnedInfere)
                  TypeRef(pre, sym, param :: nt :: Nil)
              }
            case TypeRef(pre, sym, ErrorType :: Nil) ⇒ tpe
            case t ⇒
              reporter.warning(tree.pos, "Unexpected function type: " + t)
              tpe
          }
        case Function(vparams, body) ⇒
          println("Impure function " + tree)
          //impure function, so annotate the return type with @sideEffect
          tpe match {
            case TypeRef(pre, sym, param :: result :: Nil) ⇒
              result match {
                case SideEffectType(t) ⇒ tpe
                case SideEffectFreeType(t) ⇒
                  reporter.error(tree.pos, "Impure function may not return a type annotated with @sideEffectFree: " + t)
                  tpe
                case t ⇒
                  val nt = removeAnnotation(annotateWith(t, Annotation.sideEffect), Annotation.returnedInfere)
                  TypeRef(pre, sym, param :: nt :: Nil)
              }
            case TypeRef(pre, sym, ErrorType :: Nil) ⇒ tpe
            case t ⇒
              reporter.warning(tree.pos, "Unexpected function type: " + t)
              tpe
          }
        case _ ⇒ tpe
      }
    }

    override def canAdaptAnnotations(tree: Tree, mode: Int, pt: Type): Boolean = {
      //println("# canAdapt  tree=" + tree + "  mode=" + analyzer.modeString(mode) + "  pt=" + pt + "  class=" + tree.getClass)
      if ((mode & analyzer.EXPRmode) == 0 || (mode & analyzer.LHSmode) != 0) false
      else if (phase.name == "tailcalls") false //don't do anything in the tailcall phase
      else tree match {
        case MarkMethod(_, _) ⇒ true
        case _                ⇒ false
      }
    }

    override def adaptAnnotations(tree: Tree, mode: Int, pt: Type): Tree = {
      //println("# adapt  tree=" + tree + "  mode=" + analyzer.modeString(mode) + "  pt=" + pt)
      lazy val localTyper = analyzer.newTyper(analyzer.rootContext(currentRun.currentUnit, tree, false))
      def typed(a: Tree) = dontRecurse(tree)(localTyper.typed(a))

      tree match {
        //Change markInfere to either markSideEffect, markSideEffectFree or remove it (depending on the purity on the body) 
        case MarkMethod(MarkInfere, wrapped) ⇒
          val r = if (PurityChecker.isPure(tree.symbol, wrapped)) {
            wrapped.tpe match {
              case SideEffectFreeType(_) ⇒ wrapped
              case _                     ⇒ typed(MarkMethod(MarkSideEffectFree)(wrapped))
            }
          } else {
            wrapped.tpe match {
              case SideEffectType(_) ⇒ wrapped
              case _                 ⇒ typed(MarkMethod(MarkSideEffect)(wrapped))
            }
          }
          println("##### found infere mark on " + wrapped + ": " + tree + "    => replaces with " + r)
          r

        //Remove unnecessary markSideEffect
        case MarkMethod(MarkSideEffect, wrapped) if SideEffectType.unapply(wrapped.tpe).isDefined ⇒
          println("### found sideEffect mark on " + tree + "    => replace with " + wrapped)
          wrapped
        //Remove unnecessary markSideEffectFree
        case MarkMethod(MarkSideEffectFree, wrapped) if SideEffectFreeType.unapply(wrapped.tpe).isDefined ⇒
          println("### found sideEffectFree mark on " + tree + "    => replace with " + wrapped)
          wrapped

        case _ ⇒ tree
      }
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
  }
}