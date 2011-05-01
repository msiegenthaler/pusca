package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc


class AnnotatePureComponent(val global: Global) extends PluginComponent with PureDefinitions with Transform {
	import global._
  
  val runsAfter = List("uncurry")
  val phaseName = "annotatePure"
  def newTransformer(unit: global.CompilationUnit) = {
    new AnnotatePurityTransformer
  }

  class AnnotatePurityTransformer extends Transformer {
    override def transform(t: Tree) = t match {
      //everything not declared impure is pure 
      case c: ClassDef =>
        if (!isDeclaredImpure(c.symbol)) {
          log("# annotating class " + c.symbol.fullName + " as pure")
          annotatePure(c.symbol)
          super.transform(t)
        } else c
      case d: DefDef =>
        if (!isDeclaredImpure(d.symbol)) {
          log("# annotating method " + d.symbol.fullName + " as pure")
          annotatePure(d.symbol)
          super.transform(t)
        } else d
      case f: Function =>
        if (!isDeclaredImpure(f.symbol)) {
          log("# annotating function " + f.symbol.fullName + " as pure")
          annotatePure(f.symbol)
          super.transform(t)
        } else f
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
  }
}