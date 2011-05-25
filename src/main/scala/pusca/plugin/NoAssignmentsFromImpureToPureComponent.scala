package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc
import nsc.Phase

class NoAssignmentsFromImpureToPureComponent(val global: Global) extends PluginComponent with PureDefinitions {
  import global._

  val runsAfter = List("uncurry", "rewriteImpureFunctions", "annotatePure")
  override val runsBefore = List("tailcalls", "checkPurity")
  val phaseName = "noAssignmentsFromImpureToPureComponent"
  def newPhase(_prev: Phase) = new NoAssignmentsFromImpureToPurePhase(_prev)

  class NoAssignmentsFromImpureToPurePhase(prev: Phase) extends StdPhase(prev) {
    override def name = phaseName

    def apply(unit: CompilationUnit) {
      def impureType(t: Type): Boolean = t match {
        case TypeRef(pre, sym, args) =>
          args.foldLeft(false) { (s, a) =>
            s || a.annotations.find(_.atp.typeSymbol == Annotation.impure).isDefined
          }
        case MethodType(params, res) =>
          impureType(res)
        case AnnotatedType(a, raw, _) =>
          a.find(_.atp.typeSymbol == Annotation.impure).isDefined || impureType(raw)
        case NoType => false
        case t =>
          t.parents.foldLeft(false)((s,e) => s || impureType(e))
      }

      def check(allowImpure: Boolean)(t: Tree): Unit = t match {
        case a: Apply =>
          if (!allowImpure && impureType(a.tpe))
            unit.error(t.pos, "Cannot assign impure value to a pure (impure: " + a.tpe.typeSymbol.fullName + ", apply)")
          check(allowImpure)(a.fun)
        case i: Ident =>
          if (!allowImpure && impureType(i.tpe))
            unit.error(t.pos, "Cannot assign impure value to a pure (impure: " + i.tpe.typeSymbol.fullName + ", ident)")
        case l: Literal =>
          if (!allowImpure && impureType(l.tpe))
            unit.error(t.pos, "Cannot assign impure value to a pure (impure: " + l.tpe.typeSymbol.fullName + ", literal)")
        case s: Select =>
          if (!allowImpure && impureType(s.tpe))
            unit.error(t.pos, "Cannot assign impure value to a pure (impure: " + s.tpe.typeSymbol.fullName + ", select)")

        case m: Match =>
          val impure = impureType(m.tpe)
          m.cases.foreach(check(impure))

        case Block(ss, e) =>
          ss.foreach(check(false))
          check(allowImpure)(e)

        case v: ValDef =>
          val impure = impureType(v.tpt.tpe)
          check(impure)(v.rhs)
        case d: DefDef =>
          val impure = impureType(d.tpt.tpe) || isImpure(d.symbol)
          check(impure)(d.rhs)
        case c: ClassDef =>
          check(false)(c.impl)
        case t: Template =>
          t.body.foreach(check(false))

        case other =>
          val impure = impureType(other.tpe)
          other.children.foreach(check(impure))
      }

      check(false)(unit.body)
    }
  }
}