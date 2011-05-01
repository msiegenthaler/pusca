package pusca.plugin

import scala.tools.nsc.Global

trait PureDefinitions {
  val global: Global
  import global._

  protected def log(s: String) = {
//    println(s)
  }

  protected def isDeclaredImpure(symbol: Symbol) = {
    symbol.annotations.find(_.atp.typeSymbol == Annotation.impure).isDefined
  }
  protected def onlyPureContentAllowed(symbol: Symbol) = {
    symbol.annotations.find { a =>
      a.atp.typeSymbol == Annotation.impure || a.atp.typeSymbol == Annotation.declareAsPure
    }.isEmpty
  }

  protected object Annotation {
    val pure = definitions.getClass("pusca.pure")
    val impure = definitions.getClass("pusca.impure")
    val declareAsPure = definitions.getClass("pusca.declareAsPure")
  }

  protected object Whitelist {
    val packages = Set(
      "scala.collection.immutable",
      "scala.runtime")
    val objects = Set(
      "java.lang.Object", "java.lang.String", "java.lang.Integer", "java.lang.Long",
      "scala", "scala.ScalaObject", "scala.AnyRef", "scala.Ref",
      "scala.Int", "scala.Long", "scala.Serializable", // 
      "scala.collection.Seq")
  }
}