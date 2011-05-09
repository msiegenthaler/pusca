package pusca.plugin

import scala.tools.nsc.Global

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
//    log("# " + symbol.fullName + " is considered " + (if (r) "pure" else "impure"))
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

  protected object Annotation {
    val pure = definitions.getClass("pusca.pure")
    val impure = definitions.getClass("pusca.impure")
    val declarePure = definitions.getClass("pusca.declarePure")
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