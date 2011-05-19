package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc
import nsc.symtab.Flags

class AnnotatePureComponent(val global: Global) extends PluginComponent with PureDefinitions with Transform {
  import global._

  val runsAfter = List("uncurry", "rewriteImpureFunctions")
  val phaseName = "annotatePure"
  def newTransformer(unit: global.CompilationUnit) = {
    new AnnotatePurityTransformer
  }

  def isVarAccessor(s: Symbol): Boolean = {
    s.isSetter || (s.isGetter && !s.isStable)
  }

  object VarDef {
    def unapply(t: Tree) = t match {
      case v: ValDef if ((v.symbol.flags & Flags.MUTABLE) != 0) => Some(v)
      case _ => None
    }
  }

  class AnnotatePurityTransformer extends Transformer {
    //everything not declared impure is pure
    override def transform(t: Tree) = t match {
      case ac: DefDef if isVarAccessor(ac.symbol) =>
        log("annotating var accessor " + ac.symbol.fullName + " as impure")
        annotateImpure(ac.symbol)
        super.transform(ac)
      case d: DefDef if isImpure(d.symbol) =>
        super.transform(t)
      case d: DefDef if d.symbol.isConstructor =>
        val impure = d.symbol.ownerChain.find(_.isClass).map(isImpure _).getOrElse(false)
        if (impure) {
          log("annotating constructor " + d.symbol.fullName + " as impure")
          annotateImpure(d.symbol)
        } else {
          log("annotating constructor " + d.symbol.fullName + " as pure")
          annotatePure(d.symbol)
        }
        super.transform(t)
      case d: DefDef =>
        log("annotating def " + d.symbol.fullName + " as pure")
        annotatePure(d.symbol)
        super.transform(t)
      case f: Function if isImpure(f.symbol) =>
        super.transform(t)
      case f: Function =>
        log("annotating function " + f.symbol.fullName + " as pure")
        annotatePure(f.symbol)
        super.transform(t)

      case c: ClassDef if !isImpure(c.symbol) =>
        val impure = c.impl match {
          case t: Template if t.parents.map(_.symbol).find(s => !satisfiesPureness(s)).isDefined =>
            //if a class has an impure parent it's considered impure as well
            log("annotating class (for constructors) " + c.symbol.fullName +
              " as impure, because of impure superclasses (" + t.parents.map(_.symbol).filter(s => !satisfiesPureness(s)).mkString(", ") + ")")
            annotateImpure(c.symbol)
          case t: Template if t.body.find {
            _ match {
              case VarDef(v) => true //impure
              case a: Apply if !satisfiesPureness(a.fun.symbol) => true //impure
              case a: Apply => false //pure
              case a: Assign => true //impure
              case _ => false
            }
          }.isDefined =>
            //initialization of the class results in impure calls
            log("annotating class (for constructors) " + c.symbol.fullName +
              " as impure because of impure calls in the initialization")
            annotateImpure(c.symbol)
          case _ =>
            log("annotating class (for constructors) " + c.symbol.fullName + " as pure")
            annotatePure(c.symbol)
        }
        super.transform(t)

      case o => super.transform(t)
    }
    def annotatePure(on: Symbol): Unit = {
      on.annotations.find(_.atp.typeSymbol == Annotation.pure) match {
        case None =>
          val a = AnnotationInfo(Annotation.pure.tpe, Nil, Nil)
          on.setAnnotations(a :: on.annotations)
        case Some(_) => ()
      }
    }
    def annotateImpure(on: Symbol): Unit = {
      on.annotations.find(_.atp.typeSymbol == Annotation.impure) match {
        case None =>
          val a = AnnotationInfo(Annotation.impure.tpe, Nil, Nil)
          on.setAnnotations(a :: on.annotations)
        case Some(_) => ()
      }
    }
  }
}