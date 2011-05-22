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
      case AnonFunction(c, impl, f) =>
        val impures = impureContent(c) ++ impureContent(f)
        if (impures.isEmpty) {
          log("Anonymous function in " + c.symbol.owner.fullName + " markes as pure")
          super.transform(t)
        } else {
          log("Anonymous function in " + c.symbol.owner.fullName + " markes as impure ")
          log(" because of " + impures)

          //Remove the AbstractFunctionX as the parent
          val np = impl.parents.map { p =>
            val i = abstractFunctions.indexOf(p.symbol)
            if (i > -1) {
              val TypeRef(pre, sym, params) = p.tpe
              val nr = TypeRef(pre, impureFunctions(i), params)
              TypeTree(nr)
            } else p
          }
          annotateImpure(f.symbol) //apply is now impure
          val nimpl = impl.copy(parents = np)
          copyAttrs(impl, nimpl)
          val nc = c.copy(impl = nimpl)
          copyAttrs(c, nc)
          nc
        }
      case o => super.transform(t)
    }

    private def copyAttrs(from: Tree, to: Tree) = {
      to.pos = from.pos
      to.tpe = from.tpe
      if (from.hasSymbol)
        to.symbol = from.symbol
    }
  }

  private val impureFunctions = (0 to 2).map(i => definitions.getClass("pusca.ImpureFunction" + i))
  private val abstractFunctions = (0 to 22).map(i => definitions.getClass("scala.runtime.AbstractFunction" + i))

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