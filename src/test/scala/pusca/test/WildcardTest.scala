package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class Wildcard extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def purePreservesType {
    code("@impure def x[A <: Pure[String]](f: () => A): String = f()") should compile
  }

  @Test def evaluationToPureIsPureOnType {
    code("@pure def x[A <: Pure[String]](f: () => A): String = f()") should compile
  }
  @Test def evaluationToPureIsPureOnFunction {
    code("@pure def x[A](f: () => Pure[A]) = f()") should compile
  }
  @Test def evaluationToPureIsPureWithPureFunction {
    code("@pure def x[A](f: () -> A) = f()") should compile
  }
  @Test def evaluationToPureIsPureWithPureFunction2 {
    code("@pure def x[A](f: PureFunction[Unit,A]) = f()") should compile
  }

  @Test def evaluationToNotPureIsImpure {
    code("@pure def x[A <: String](f: () => A): String = f()") should
      yieldCompileError("impure method call to apply inside the pure method x")
  }

  @Test def funWithPureReturnTypeAllowsPureFunctionOnType {
    code("""
        @pure def x[A <: Pure[Int]](f: String => A): Int = f("Hello")
        @pure def p = x(_.length)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunctionOnType2 {
    code("""
        @pure def x[A <: Pure[Int]](f: String => A): Int = f("Hello")
        @pure def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunctionOnType3 {
    code("""
        @pure def x[A <: Pure[Int]](f: String => A): Int = f("Hello")
        def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunction {
    code("""
        @pure def x[A <: Int](f: String -> A): Int = f("Hello")
        @pure def p = x(_.length)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunction2 {
    code("""
        @pure def x[A <: Int](f: String -> A): Int = f("Hello")
        @pure def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunction3 {
    code("""
        @pure def x[A <: Int](f: String -> A): Int = f("Hello")
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

  @Test def canAssignSideEffectFreeToPure {
    code("""
				class A[X <: Pure[Int]](val f: String => X) {}
				@pure def len(a: String) = 10
				@pure def p = new A(len)
			""") should compile
  }

  @Test def cannotAssignSideEffectToPure {
    code("""
        class A[X <: Pure[Int]](val f: String => X) {}
        @impure def len(a: String) = 10
        @pure def p = new A(len)
        """) should yieldCompileError("type mismatch")
  }
  @Test def cannotAssignSideEffectToPure2 {
    code("""
  			class A[X <: Pure[Int]](val f: String => X) {}
  			@impure def len(a: String) = 10
  			@pure def p = new A[Int @sideEffect](len)
  		""") should yieldCompileError("type mismatch")
  }

  @Test def cannotAssignSideEffectFunToPureFun {
    code("""
        val f: String => Int @sideEffect = (v: String) => v.length
        val g: String => Pure[Int] = f
        """) should yieldCompileError("type mismatch")
  }

  @Test def canAssignNormalToPure {
    code("""
  	    val a: Pure[String] = "Hello"
  			""") should compile
  }
  @Test def canAssignPureToNormal {
    code("""
  			val a: Pure[String] = "Hello"
  	    val b: String = a
  	""") should compile
  }
  @Test def canAssignPureToSideEffect {
    code("""
  	    val a: String => Pure[Int] = (v: String) => v.length
  	    val f: String => Int @sideEffect = a
  			""") should compile
  }
}