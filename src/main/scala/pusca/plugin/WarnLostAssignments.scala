package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc
import nsc.Phase

class WarnLostAssignments(val global: Global) extends PluginComponent with PureDefinitions {
  import global._

  val runsAfter = List("uncurry", "checkPurity", "annotatePure")
  override val runsBefore = List("tailcalls")
  val phaseName = "lostAssignments"
  def newPhase(_prev: Phase) = new WarnLostAssignmentsPhase(_prev)

  object Types {
    val unit = definitions.getClass("scala.Unit")
    val nothing = definitions.getClass("scala.Nothing")
  }

  def isNoType(t: Symbol) = {
    t == Types.unit || t == Types.nothing
  }

  class WarnLostAssignmentsPhase(prev: Phase) extends StdPhase(prev) {
    override def name = phaseName
    def apply(unit: CompilationUnit) {
      def check(t: Tree): Unit = t match {
        case c: ClassDef =>
          if (onlyPureContentAllowed(c.symbol)) checkPure(c.impl)
          else t.children.foreach(check _)
        case t: Template =>
          t.body.foreach(check _)
        case d: DefDef =>
          if (onlyPureContentAllowed(d.symbol)) checkPure(d.rhs)
          else t.children.foreach(check _)
        case v: ValDef =>
          checkPure(v.rhs)
        case p: PackageDef =>
          p.stats.foreach(check _)
        case other =>
          other.children.foreach(check _)
      }

      def checkPure(t: Tree): Unit = {
        log("Checking pure " + t.getClass + " (" + t.tpe.typeSymbol.fullName + ")")
        t match {
          case d: DefDef =>
            check(d)
          case v: ValDef =>
            check(v)
          case c: ClassDef =>
            check(c)
          case b: Block =>
            // log("# Block with expression '" + b.expr + "' and statements '" + b.stats+"'")
            b.stats.foreach(checkLost _)
            checkPure(b.expr)
          case t: Template =>
            t.body.foreach(checkLost _)
          case c: CaseDef =>
            checkPure(c.body)
          case m: Match =>
            checkPure(m.selector)
            m.cases.foreach(checkPure _)
          case _: Ident => ()
          case _: Literal => ()
          case _: Select => ()
          case _: Apply => ()
          case other =>
            log("Unexpected tree in checkPure " + other.getClass + " (" + other + ")")
            ()
        }
      }

      object SuperInit {
        def unapply(t: Tree) = t match {
          case a: Apply => a.fun match {
            case Select(q, n: TermName) if n.toString == "<init>" => q.children match {
              case (th: This) :: _ =>
                Some(t)
              case _ => None
            }
            case _ => None
          }
          case _ => None
        }
      }

      def checkLost(t: Tree): Unit = {
        log("# Checking lost " + t.getClass + " (" + t.tpe.typeSymbol.fullName + ")")
        t match {
          case SuperInit(c) =>
            //ignore calls to super constructor
            ()
          case a: Apply =>
            unit.warning(t.pos, "Result of call is not used (type: " + a.tpe.typeSymbol.fullName + ") [a]")
          case s: Select =>
            unit.warning(t.pos, "Result of call is not used (type: " + s.tpe.typeSymbol.fullName + ") [s]")
          case b: Block =>
            b.stats.foreach(checkLost _)
            if (!isNoType(b.expr.tpe.typeSymbol))
              unit.warning(t.pos, "Result of block is not used (type: " + b.expr.tpe.typeSymbol.fullName + ")")
          case l: Literal =>
            unit.warning(t.pos, "Literal is not used (type: " + l.tpe.typeSymbol.fullName + ", value '" + l.value + "')")
          case m: Match =>
            unit.warning(t.pos, "Result of match is not used (type: " + m.tpe.typeSymbol.fullName + ")")
          case d: DefDef => check(d)
          case v: ValDef => check(v)
          case c: ClassDef => check(c)
          case o =>
            log("Unexpected tree in checkLost: " + o.getClass + " (" + o + ")")
            o.children.foreach(checkLost _)
        }
      }

      check(unit.body)
    }

  }
}