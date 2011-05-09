package pusca.plugin

import scala.tools.nsc.Global

trait PureDefinitions {
  val global: Global
  import global._

    protected def log(s: String) = {
      println(s)
    }

  protected def isDeclaredImpure(symbol: Symbol) = {
    symbol.annotations.find(_.atp.typeSymbol == Annotation.impure).isDefined
  }
  protected def onlyPureContentAllowed(symbol: Symbol) = {
    symbol.annotations.find { a =>
      a.atp.typeSymbol == Annotation.impure || a.atp.typeSymbol == Annotation.declarePure
    }.isEmpty
  }

  protected object Annotation {
    val pure = definitions.getClass("pusca.pure")
    val impure = definitions.getClass("pusca.impure")
    val declarePure = definitions.getClass("pusca.declarePure")
  }

  protected object Whitelist {
    val packages = Set(
      "scala.collection.immutable",
      "scala.runtime")
      
    val objects = Set(
      "java.lang.Object", "java.lang.String", "java.lang.Integer", "java.lang.Long", "java.lang.Char", "java.lang.Float", "java.lang.Double",
      "scala", "scala.ScalaObject", "scala.AnyRef", "scala.AnyVal", "scala.Ref", "scala.AnyValCompanion", //
      "scala.Int", "scala.Long", "scala.Char", "scala.BigInt", "scala.BigDecimal", "scala.Boolean", "scala.Byte", "scala.Numeric", //
      "scala.Serializable", "scala.::", "scala.List", "scala.Equals", "scala.Proxy", "scala.Unit", // 
      "scala.Either", "scala.Left", "scala.Right", "scala.Option", "scala.Some", "scala.Nothing",  //
      "scala.Product", // 
      "scala.Tuple", //
      "scala.Function", //
      "java.lang.Exception", "java.lang.Error", "java.lang.RuntimeException", "java.lang.IndexOutOfBoundsException",
      "scala.collection.Seq")
  }
}