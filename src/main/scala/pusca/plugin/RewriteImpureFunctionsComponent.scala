package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc
import nsc.symtab.Flags

class RewriteImpureFunctionsComponent(val global: Global) extends PluginComponent with PureDefinitions with Transform {
  import global._

  val runsAfter = List("uncurry", "annotatePure")
  val phaseName = "rewriteImpureFunctions"
  def newTransformer(unit: global.CompilationUnit) = {
    new AnnotatePurityTransformer
  }

  class AnnotatePurityTransformer extends Transformer {
    override def transform(t: Tree) = t match {
      case AnonFunction(c, impl, f) =>
        val impures = impureContent(c) ++ impureContent(f)
        if (impures.isEmpty) {
          log("Anonymous function in " + c.symbol.owner.fullName + " markes as pure")
          super.transform(t)
        } else {
          log("Anonymous function in " + c.symbol.owner.fullName + " markes as impure ")
          log(" because of " + impures)

          //Change extended class from Function[A,B] to Function[A,B @impure]
          val np = impl.parents.map { p =>
            if (abstractFunctions.contains(p.symbol)) annotateFunctionClassReturnType(p)
            else p
          }

          //Change apply from apply(a: A): B to @impure apply(a: A): B
          annotateImpure(f.symbol)
          
          
//          val ntpt = TypeTree().defineType(annotateTypeWithImpure(f.tpt.tpe))
//          val nf = f.copy(tpt = ntpt)
//          nf.pos = f.pos
//          nf.symbol = f.symbol
//          nf.symbol = f.symbol.owner.newMethod(nf.name, nf.pos).initialize
//          f.symbol.name = nf.name
//          nf.symbol = f.symbol
//          copyAttrs(f, nf)
//          val nbody = nf :: impl.body.filterNot(_ == f)

          //TODO necessary
          //          annotateImpure(c.symbol) // class is now impure

          val nimpl = impl.copy(parents = np)
          copyAttrs(impl, nimpl)
          val nc = c.copy(impl = nimpl)
          copyAttrs(c, nc)
          nc
        }
      case o => super.transform(t)
    }

    private def annotateFunctionClassReturnType(p: Tree) = {
      val TypeRef(pre, sym, params) = p.tpe
      val rt = params.last
      val nrt = annotateTypeWithImpure(rt)
      val nparams = params.take(params.length - 1) ::: nrt :: Nil

      val nr = TypeRef(pre, sym, nparams)
      log("## impure anon-function: changed " + p.tpe + " to " + nr)
      TypeTree(nr)
    }
    private def annotateTypeWithImpure(t: Type) = t match {
      case AnnotatedType(annots, raw, s) =>
        val nannots = AnnotationInfo(Annotation.impure.tpe, Nil, Nil) :: annots.filterNot(Annotation.purenessAnnotation)
        AnnotatedType(nannots, raw, s)
      case t =>
        AnnotatedType(AnnotationInfo(Annotation.impure.tpe, Nil, Nil) :: Nil, t, NoSymbol)
    }

    private def copyAttrs(from: Tree, to: Tree) = {
      to.pos = from.pos
      to.tpe = from.tpe
      if (from.hasSymbol)
        to.symbol = from.symbol
    }
  }

  object AnonFunction {
    def unapply(t: Tree) = t match {
      case c @ ClassDef(_, _, _, impl) if c.symbol.isAnonymousFunction =>
        val applyFun = impl.body.find {
          _ match {
            case ApplyFunction(d) => true
            case _ => false
          }
        }
        applyFun.map(f => (c, impl, f.asInstanceOf[DefDef]))
      case _ => None
    }
  }

  object ApplyFunction {
    def unapply(t: Tree) = t match {
      case d: DefDef if d.name.toString == "apply" => Some(d)
      case _ => None
    }
  }
}