package pusca.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.ast.TreeBrowsers
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.symtab.Flags
import nsc.transform.Transform

class PurePlugin(val global: Global) extends Plugin {
  import global._

  val whitelistPackages = Set(
    "scala.collection.immutable")
  val whilelistObjects = Set(
    "java.lang.Object", "java.lang.String", "java.lang.Integer", "java.lang.Long",
    "scala", "scala.ScalaObject", "scala.AnyRef", "scala.Ref",
    "scala.Int", "scala.Long")

  val name = "pure"
  val description = "Enforces pureness"
  val components = List[PluginComponent](AnnotatePureComponent, CheckPurityComponent)

  val annotPure = definitions.getClass("pusca.pure")
  val annotImpure = definitions.getClass("pusca.impure")
  val annotDeclarePure = definitions.getClass("pusca.declareAsPure")

  val unitT = definitions.getClass("scala.Unit")
  val nothingT = definitions.getClass("scala.Nothing")

  private def isDeclaredImpure(symbol: Symbol) = {
    symbol.annotations.find(_.atp.typeSymbol == annotImpure).isDefined
  }
  private def onlyPureContentAllowed(symbol: Symbol) = {
    symbol.annotations.find { a =>
      a.atp.typeSymbol == annotImpure || a.atp.typeSymbol == annotDeclarePure
    }.isEmpty
  }
  private def isInvalidReturnType(symbol: Symbol) = {
    symbol == unitT || symbol == nothingT
  }

  private def log(s: String) = {
    //    println(s)
  }

  private object AnnotatePureComponent extends PluginComponent with Transform {
    val global: PurePlugin.this.global.type = PurePlugin.this.global
    val runsAfter = List("refchecks")
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
            log("# annotating function " + d.symbol.fullName + " as pure")
            annotatePure(d.symbol)
            super.transform(t)
          } else d
        case o => super.transform(t)
      }
      def annotatePure(on: Symbol): Unit = {
        on.annotations.find(_.atp.typeSymbol == annotPure) match {
          case None =>
            val a = AnnotationInfo(annotPure.tpe, Nil, Nil)
            on.setAnnotations(a :: on.annotations)
          case Some(_) => ()
        }
      }
    }
  }
  private object CheckPurityComponent extends PluginComponent {
    val global: PurePlugin.this.global.type = PurePlugin.this.global
    val runsAfter = List("refchecks")
    val phaseName = "checkPurity"
    def newPhase(_prev: Phase) = new CheckPurityPhase(_prev)

    class CheckPurityPhase(prev: Phase) extends StdPhase(prev) {
      override def name = "checkPurity"

      //TODO also check for 'lost' assignments (warn), because they don't make sense in a pure function

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
              unit.error(d.pos, "A pure class must not have impure members")
            else if (onlyPureContentAllowed(d.symbol)) {
              log("# asserting purity of def " + d.name)
              d.children.foreach(checkPurity(true) _)
            }
          case v: ValDef =>
            log("# asserting purity of val " + v.name)
            if (isDeclaredImpure(v.symbol))
              unit.error(v.pos, "A pure class must not have impure members")
            else if (onlyPureContentAllowed(v.symbol)) {
              if ((v.symbol.flags & Flags.MUTABLE) != 0) //TODO allow them in defs?
                unit.error(v.pos, "No vars allowed in pure classes") //TODO really? or only no access to them in pure methods
              else
                v.children.foreach(checkPurity(true) _)
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

        //        global.treeBrowsers.create().browse(unit.body)
      }

      def isPure(t: Symbol): Boolean = {
        t match {
          case NoSymbol => true
          case symbol =>
            val p = if (t.annotations.find(_.atp.typeSymbol == annotPure).isDefined) true
            else {
              val fn = t.fullName
              if (whilelistObjects.contains(fn)) true
              else whitelistPackages.find(fn.startsWith _).isDefined
            }
            if (!p)
              log("@    " + t + " (" + t.fullName + ") is impure")
            p
        }
      }
    }
  }
}