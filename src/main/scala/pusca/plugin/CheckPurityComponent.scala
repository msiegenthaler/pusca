package pusca.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.symtab.Flags

class CheckPurityComponent(val global: Global) extends PluginComponent with PureDefinitions {
  import global._

  val runsAfter = List("uncurry", "annotatePure")
  override val runsBefore = List("tailcalls")
  val phaseName = "checkPurity"
  def newPhase(_prev: Phase) = new CheckPurityPhase(_prev)

  class CheckPurityPhase(prev: Phase) extends StdPhase(prev) {
    override def name = phaseName

    def apply(unit: CompilationUnit) {
      def checkPurity(inDef: Boolean)(e: Tree): Unit = e match {
        case t: Template =>
          t.parents.foreach { p =>
            if (!isPure(p.symbol)) unit.error(t.pos, "A pure class must not extend the impure class " + p.symbol.fullName)
          }
          if (!isPure(t.self.symbol)) unit.error(t.pos, "Self of a pure class must also be pure")
          t.children.foreach(checkPurity(false) _)

        case d: DefDef =>
          if (isDeclaredImpure(d.symbol))
            unit.error(d.pos, "A pure class must not have impure defs")
          else if (onlyPureContentAllowed(d.symbol)) {
            log("# asserting purity of def " + d.name)
            d.children.foreach(checkPurity(true) _)
          }
        case f: Function =>
          if (isDeclaredImpure(f.symbol))
            unit.error(f.pos, "A pure class must not have impure functions")
          else if (onlyPureContentAllowed(f.symbol)) {
            log("# asserting purity of fun")
            f.children.foreach(checkPurity(true) _)
          }
        case v: ValDef =>
          log("# asserting purity of val " + v.name)
          if (isDeclaredImpure(v.symbol))
            unit.error(v.pos, "A pure class must not have impure vals")
          else if (onlyPureContentAllowed(v.symbol)) {
            if ((v.symbol.flags & Flags.MUTABLE) != 0) //TODO allow them in defs?
              unit.error(v.pos, "No vars allowed in pure classes") //TODO really? or only no access to them in pure methods
            else
              v.rhs.foreach(checkPurity(true) _)
          }

        case i: Ident =>
          def findSuperOf(s: Symbol): Symbol = s match {
            case NoSymbol => s
            case s if !s.isLocal => s
            case o => findSuperOf(s.owner)
          }
          val s = findSuperOf(i.symbol)
          log("##    resolved " + i.symbol.fullName + " to " + s.fullName)
          if (!isPure(s)) unit.error(i.pos, "Cannot reference impure " + i.symbol.fullName + " inside pure classes")
          i.children.foreach(checkPurity(inDef) _)

        case a: TypeApply =>
          log("#   asserting purity of apply " + a)
          if (!isPure(a.tpe.typeSymbol))
            unit.error(a.pos, "impure content in pure function")
          else
            a.children.foreach(checkPurity(true) _)
        case a: Apply =>
          log("#   asserting purity of apply " + a)
          if (!isPure(a.tpe.typeSymbol))
            unit.error(a.pos, "impure content in pure function")
          else
            a.children.foreach(checkPurity(true) _)

        case c: ClassDef =>
          if (isDeclaredImpure(c.symbol))
            unit.error(c.pos, "Cannot defined impure classes inside a pure one")
          else if (onlyPureContentAllowed(c.symbol))
            c.children.foreach(checkPurity(false) _)

        case o =>
          o.children.foreach(checkPurity(inDef) _)
      }

      def checkPurityTop(e: Tree): Unit = e match {
        case c: ClassDef =>
          if (onlyPureContentAllowed(c.symbol)) {
            log("# asserting purity of class " + c.name)
            checkPurity(false)(c)
          }
        case o =>
          o.children.foreach(checkPurityTop _)
      }

      checkPurityTop(unit.body)
    }

    def isPure(t: Symbol): Boolean = {
      log("@@ checking " + t.fullName + " (" + t + ") (" + t.flags + ")")
      t match {
        case NoSymbol => true
        case symbol if symbol.isRefinementClass => isPure(symbol.owner)
        case symbol =>
          val p = if (t.annotations.find(_.atp.typeSymbol == Annotation.pure).isDefined) true
          else {
            val fn = t.fullName
            if (Whitelist.objects.contains(fn)) true
            else Whitelist.packages.find(fn.startsWith _).isDefined
          }
          if (!p)
            log("@    " + t + " (" + t.fullName + ") is impure")
          p
      }
    }
  }
}