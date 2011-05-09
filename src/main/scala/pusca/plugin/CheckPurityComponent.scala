package pusca.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class CheckPurityComponent(val global: Global) extends PluginComponent with PureDefinitions {
  import global._

  val runsAfter = List("uncurry", "annotatePure")
  override val runsBefore = List("tailcalls")
  val phaseName = "checkPurity"
  def newPhase(_prev: Phase) = new CheckPurityPhase(_prev)

  class CheckPurityPhase(prev: Phase) extends StdPhase(prev) {
    override def name = phaseName

    def apply(unit: CompilationUnit) {

      def checkPure(enclosingFun: Symbol)(t: Tree): Unit = t match {
        case a: Apply =>
          if (!satisfiesPureness(a.fun.symbol))
            unit.error(t.pos, "Impure function call to " + a.fun.symbol.fullName + " inside the pure function " + enclosingFun.fullName)
          else a.args.foreach(checkPure(enclosingFun))

        case a: Assign =>
          if (!a.lhs.symbol.ownerChain.contains(enclosingFun))
            unit.error(t.pos, "Pure function " + enclosingFun.fullName + " contains assignment to non-enclosed var " + a.lhs.symbol.fullName)
          else a.rhs.foreach(checkPure(enclosingFun))

        case d: DefDef =>
          process(d)
        case o =>
          o.children.foreach(checkPure(enclosingFun))

      }

      def process(t: Tree): Unit = t match {
        case DeclaredPureFunction(rhs) =>
          log("Assuming purity of " + t.symbol.fullName)
          process(rhs)

        case PureFunction(rhs) =>
          log("Validating purity of " + t.symbol.fullName)
          checkPure(t.symbol)(rhs)

        case d @ ImpureFunction(rhs) =>
          log("Validating if " + d.symbol.fullName + " is allowed to be impure")
          val pureOverrides = d.symbol.allOverriddenSymbols.filter(satisfiesPureness)
          if (!pureOverrides.isEmpty) {
            if (pureOverrides.size == 1)
              unit.error(d.pos, "Impure function " + d.symbol.fullName + " cannot override pure function " + pureOverrides.head.fullName)
            else
              unit.error(d.pos, "Impure function " + d.symbol.fullName + " cannot override pure functions " + pureOverrides.map(_.fullName).mkString(", "))
          } else
            process(rhs)

        //TODO check if really impure and warn otherwise?

        case o => o.children.foreach(process _)
      }

      //TODO what's with functions as arguments?

      process(unit.body)
    }
  }
}