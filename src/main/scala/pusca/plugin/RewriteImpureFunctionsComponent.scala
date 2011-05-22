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

  //TODO
  protected override def log(s: => String) = println(s)

  class AnnotatePurityTransformer extends Transformer {
    override def transform(t: Tree) = t match {
      case AnonFunction(c, f) =>
        val impures = impureContent(c) ++ impureContent(f)
        if (impures.isEmpty) {
          log("Anonymous function in " + c.symbol.owner.fullName + " markes as pure")
          super.transform(t)
        } else {
          log("Anonymous function in " + c.symbol.owner.fullName + " markes as impure ")
          log(" because of " + impures)
          super.transform(t)
        }

      case o => super.transform(t)
    }
  }

  object AnonFunction {
    def unapply(t: Tree) = t match {
      case c: ClassDef if c.symbol.isAnonymousFunction =>
        val applyFun = c.impl.children.find {
          _ match {
            case d: DefDef if d.name.toString == "apply" =>
              //TODO
              true
            case _ => false
          }
        }
        applyFun.map(f => (c, f))
      case _ => None
    }
  }
}