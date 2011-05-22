package pusca.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags

trait PureDefinitions {
  val global: Global
  import global._

  protected def log(s: => String) = {
    //    println(s)
  }

  object PureFunction {
    def unapply(t: Tree) = t match {
      case d: DefDef if isPure(d.symbol) =>
        Some(d.rhs)
      case _ => None
    }
  }
  object DeclaredPureFunction {
    def unapply(t: Tree) = t match {
      case d: DefDef if isDeclaredPure(d.symbol) =>
        Some(d.rhs)
      case _ => None
    }
  }
  object ImpureFunction {
    def unapply(t: Tree) = t match {
      case d: DefDef if isImpure(d.symbol) =>
        Some(d.rhs)
      case _ => None
    }
  }
  def isPure(s: Symbol) = s.annotations.find(_.atp.typeSymbol == Annotation.pure).isDefined
  def isImpure(s: Symbol) = s.annotations.find(_.atp.typeSymbol == Annotation.impure).isDefined
  def isDeclaredPure(s: Symbol) = s.annotations.find(_.atp.typeSymbol == Annotation.declarePure).isDefined
  def onlyPureContentAllowed(symbol: Symbol) = {
    symbol.annotations.find { a =>
      a.atp.typeSymbol == Annotation.impure || a.atp.typeSymbol == Annotation.declarePure
    }.isEmpty
  }

  private def packageOfSymbol(s: Symbol): Symbol = s match {
    case NoSymbol => NoSymbol
    case s if s.isPackage || s.isPackageClass => s
    case s => packageOfSymbol(s.owner)
  }
  private def classOfSymbol(s: Symbol): Symbol = s match {
    case NoSymbol => NoSymbol
    case s if s.isClass => s
    case s => classOfSymbol(s.owner)
  }

  def isOnPurenessWhitelist(symbol: Symbol): Boolean = {
    val p = packageOfSymbol(symbol).fullName
    val c = classOfSymbol(symbol).fullName
    val f = symbol.fullName

    def objectWhitelist = Whitelist.objects.contains(c)
    def packageWhitelist = Whitelist.packages.contains(p) && !Blacklist.objects.contains(c) && !Blacklist.funs.contains(f)

    val r = objectWhitelist || packageWhitelist
    r
  }
  def satisfiesPureness(s: Symbol): Boolean = s match {
    case NoSymbol => true
    case symbol if isPure(symbol) => true
    case symbol if isDeclaredPure(symbol) => true
    case symbol if isImpure(symbol) => false
    case symbol if isOnPurenessWhitelist(symbol) => true
    case symbol if symbol.isRefinementClass => isOnPurenessWhitelist(s)
    case _ => false
  }

  val abstractFunctions = (0 to 22).map(i => definitions.getClass("scala.runtime.AbstractFunction" + i))

  def isVarAccessor(s: Symbol): Boolean = {
    s.isSetter || (s.isGetter && !s.isStable)
  }
  object VarDef {
    def unapply(t: Tree) = t match {
      case v: ValDef if ((v.symbol.flags & Flags.MUTABLE) != 0) => Some(v)
      case _ => None
    }
  }

  def annotatePure(on: Symbol): Unit = {
    val a = AnnotationInfo(Annotation.pure.tpe, Nil, Nil)
    val na = a :: on.annotations.filterNot(Annotation.purenessAnnotation)
    on.setAnnotations(na)
  }
  def annotateImpure(on: Symbol): Unit = {
    val a = AnnotationInfo(Annotation.impure.tpe, Nil, Nil)
    val na = a :: on.annotations.filterNot(Annotation.purenessAnnotation)
    on.setAnnotations(na)
  }

  /** Checks if the Tree is impure and returns all its impure content */
  def impureContent(t: Tree): List[Tree] = {
    def enclosingFun(s: Symbol): Symbol = s match {
      case NoSymbol => NoSymbol
      case s if s.isClass => NoSymbol
      case s if s.isMethod => s
      case s => enclosingFun(s.owner)
    }
    def exec(t: Tree, outermost: Boolean = false): List[Tree] = {
      def handleAll(t: Traversable[Tree]) =
        t.foldLeft[List[Tree]](Nil)((l, e) => exec(e) ::: l)

      t match {
        case Block(s, e) =>
          handleAll(s :+ e)
        case a: Apply =>
          val l = handleAll(a.args)
          if (!satisfiesPureness(a.fun.symbol)) a.fun :: l
          else l
        case a: Assign =>
          val l = exec(a.rhs)
          val ef = enclosingFun(a.symbol)
          if (!a.lhs.symbol.ownerChain.contains(ef)) a :: l
          else l
        case t: Template =>
          val l = t.parents.filter(t => !satisfiesPureness(t.symbol))
          t.body.foldLeft(l) { (l, t) =>
            t match {
              case VarDef(v) => t :: l
              case t: Assign => t :: l // all assigns in template body are bad
              case t => exec(t) ::: l
            }
          }

        // don't go into these things
        case c: ClassDef if outermost => exec(c.impl)
        case d: DefDef if outermost => exec(d.rhs)
        case v: ValDef if outermost => exec(v.rhs)
        case _ => Nil
      }
    }
    exec(t, true).reverse
  }

  protected object Annotation {
    val pure = definitions.getClass("pusca.pure")
    val impure = definitions.getClass("pusca.impure")
    val declarePure = definitions.getClass("pusca.declarePure")
    def purenessAnnotation(ai: AnnotationInfo) = {
      val a = ai.atp.typeSymbol
      a == pure || a == impure || a == declarePure
    }
  }

  protected object Whitelist {
    val packages = Set(
      "java.lang", // special classes are blacklisted
      "scala", //special classes are blacklisted
      "scala.runtime",
      "scala.annotation",
      "scala.annotation.target",
      "scala.annotation.unchecked",
      "scala.collection",
      "scala.collection.immutable",
      "scala.collection.parallel.immutable",
      "scala.math")

    val objects = Set(
      "xxxx") ++ // 
      Nil
  }

  protected object Blacklist {
    val objects = Set(
      //java.lang
      "java.lang.Appendable", "java.lang.Readable", "java.lang.ClassLoader", "java.lang.Compiler", "java.lang.InheritableThreadLocal",
      "java.lang.Process", "java.lang.ProcessBuilder", "java.lang.Runtime", "java.lang.StringBuilder", "java.lang.StringBuffer",
      "java.lang.System", "java.lang.Thread", "java.lang.ThreadGroup", "java.lang.ThreadLocal", //
      //scala
      "scala.Console")

    val funs = Set(
      //scala
      "scala.Predef.println", "scala.Predef.print", "scala.Predef.printf", "scala.Predef.readBoolean", "scala.Predef.readByte",
      "scala.Predef.readChar", "scala.Predef.readDouble", "scala.Predef.readFloat", "scala.Predef.readInt", "scala.Predef.readLine",
      "scala.Predef.readLong", "scala.Predef.readShort", "scala.Predef.readf", "scala.Predef.readf1", "scala.Predef.readf2",
      "scala.Predef.exit")
  }
}