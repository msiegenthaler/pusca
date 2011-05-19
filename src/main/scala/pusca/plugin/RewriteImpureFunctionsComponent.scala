package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc
import nsc.symtab.Flags

class RewriteImpureFunctionsComponent(val global: Global) extends PluginComponent with PureDefinitions with Transform {
  import global._

  val runsAfter = List("uncurry")
  val phaseName = "rewriteImpureFunctions"
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
    override def transform(t: Tree) = t match {
      case AnonFunction(c) =>
        println("Found an anon function in " + c.symbol.owner.fullName)
        super.transform(t)
      case o => super.transform(t)
    }
  }

  object AnonFunction {
    def unapply(t: Tree) = t match {
      case c: ClassDef if c.symbol.isAnonymousClass && c.impl.parents.find(p => extendsFunction(p.symbol)).isDefined =>
        Some(c)
      case _ => None
    }

    private val abstractFunctions = (1 to 22).map { i =>
      definitions.getClass("scala.runtime.AbstractFunction" + i)
    }
    private def extendsFunction(s: Symbol) = abstractFunctions.find(_ == s).isDefined
  }
}