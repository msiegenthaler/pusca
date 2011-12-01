package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class Wildcard extends JUnitSuite with ShouldMatchersForJUnit {

  @Test def purePreservesType {
    code("@impure def x[A <: Pure[String]](f: () => A): String = f()") should compile
  }

  @Test def evaluationToPureIsPure {
    code("@pure def x[A <: Pure[String]](f: () => A): String = f()") should compile
  }
  @Test def evaluationToNotPureIsImpure {
    code("@pure def x[A <: String](f: () => A): String = f()") should
      yieldCompileError("impure method call to apply inside the pure method x")
  }

  @Test def funWithPureReturnTypeAllowsPureFunction {
    code("""
        @pure def x[A <: Pure[Int]](f: String => A): Int = f("Hello")
        @pure def p = x(_.length)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunction2 {
    code("""
        @pure def x[A <: Pure[Int]](f: String => A): Int = f("Hello")
        @pure def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunction3 {
    code("""
        @pure def x[A <: Pure[Int]](f: String => A): Int = f("Hello")
        def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }

  @Test def funWithPureReturnTypeDoesNotAllowImpureFunction {
    code("""
        @pure def x[A <: Pure[Int]](f: String => A): Int = f("Hello")
        @impure def len(x: String) = x.length
        @pure def p = x[Int](len)
        """) should yieldCompileError("type mismatch")
  }
  @Test def funWithPureReturnTypeDoesNotAllowImpureFunction2 {
  	code("""
  			@pure def x[A <: Pure[Int]](f: String => A): Int = f("Hello")
  			@impure def len(x: String) = x.length
  			@pure def p = x[Int @sideEffect](len)
  	""") should yieldCompileError("type mismatch")
  }
  
  @Test def funWithoutPureReturnTypeDoesNotAllowImpureFunction {
    code("""
        @impure def x[A <: Int](f: String => A): Int = f("Hello")
        @impure def len(x: String) = x.length
        def p = x[Int @sideEffect](len)
        """) should compile
  }

}