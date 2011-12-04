package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class RequirePureTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def evaluationToPureIsPure {
    code("@pure @requirePure('A) def x[A](f: () => A): A = f()") should compile
  }
  @Test def evaluationToPureIsPureRestriction {
    code("@pure @requirePure('A) def x[A <: String](f: () => A): String = f()") should compile
  }
  @Test def evaluationToNotPureIsImpure {
    code("@pure def x[A](f: () => A): String = f()") should
      yieldCompileError("impure method call to apply inside the pure method x")
  }

  @Test def funWithPureReturnTypeAllowsPureFunction {
    code("""
        @pure @requirePure('A) def x[A](f: String => A): A = f("Hello")
        @pure def p = x(_.length)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunction2 {
    code("""
        @pure @requirePure('A) def x[A](f: String => A): A = f("Hello")
        @pure def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunction3 {
    code("""
        @pure @requirePure('A) def x[A](f: String => A): A = f("Hello")
        def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunctionRestriction {
    code("""
        @pure @requirePure('A) def x[A <: Int](f: String => A): Int = f("Hello")
        @pure def p = x(_.length)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunctionRestriction2 {
    code("""
        @pure @requirePure('A) def x[A <: Int](f: String => A): Int = f("Hello")
        @pure def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunctionRestriction3 {
    code("""
        @pure @requirePure('A) def x[A <: Int](f: String => A): Int = f("Hello")
        def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }

  @Test def funWithPureReturnTypeDoesNotAllowImpureFunction {
    code("""
        @pure @requirePure('A) def x[A](f: String => A): A = f("Hello")
        @impure def len(x: String) = x.length
        @pure def p = x[Int](len)
        """) should yieldCompileError("type mismatch")
  }
  @Test def funWithPureReturnTypeDoesNotAllowImpureFunction2 {
    code("""
  			@pure  @requirePure('A) def x[A](f: String => A): A = f("Hello")
  			@impure def len(x: String) = x.length
  			@pure def p = x[Int @sideEffect](len)
  		""") should yieldCompileError("type mismatch")
  }
  @Test def funWithPureReturnTypeDoesNotAllowImpureFunctionRestriction {
    code("""
        @pure @requirePure('A) def x[A <: Int](f: String => A): Int = f("Hello")
        @impure def len(x: String) = x.length
        @pure def p = x[Int](len)
        """) should yieldCompileError("type mismatch")
  }
  @Test def funWithPureReturnTypeDoesNotAllowImpureFunctionRestriction2 {
    code("""
  			@pure  @requirePure('A) def x[A <: Int](f: String => A): Int = f("Hello")
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
		    @requirePure('A)
				class A[X <: Int](val f: String => X) {}
				@pure def len(a: String) = 10
				@pure def p = new A(len)
			""") should compile
  }

  @Test def cannotAssignSideEffectToPure {
    code("""
        @requirePure('A)
        class A[X <: Int](val f: String => X) {}
        @impure def len(a: String) = 10
        @pure def p = new A(len)
        """) should yieldCompileError("type mismatch")
  }
  @Test def cannotAssignSideEffectToPure2 {
    code("""
  	    @requirePure('A)
  			class A[X <: Int](val f: String => X) {}
  			@impure def len(a: String) = 10
  			@pure def p = new A[Int @sideEffect](len)
  		""") should yieldCompileError("type mismatch")
  }
}