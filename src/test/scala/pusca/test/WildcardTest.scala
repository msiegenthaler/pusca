package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class Wildcard extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def normalTypeParameter {
    code("@pure def x[A <: String](f: () => A): String = f()") should compile
  }

  @Test def sideEffectTypeParameter {
    code("@impure def x[A <: String @sideEffect](f: () => A): String = f()") should compile
  }

  @Test def sideEffectTypeParameterIsNotPure {
    code("@pure def x[A <: String @sideEffect](f: () => A): String = f()") should
      yieldCompileError("call to impure method apply in pure method x")
  }

  @Test def purePreservesType {
    code("@impure def x[A <: String](f: () => A): String = f()") should compile
  }

  @Test def evaluationToPureIsPure {
    code("@pure def x[A <: String](f: () => A): String = f()") should compile
  }
  @Test def evaluationToNotPureIsImpure {
    code("@pure def x[A <: String @sideEffect](f: () => A): String = f()") should
      yieldCompileError("impure method call to apply inside the pure method x")
  }

  @Test def funWithPureReturnTypeAllowsPureFunction {
    code("""
        @pure def x[A <: Int](f: String => A): Int = f("Hello")
        @pure def p = x(_.length)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunction2 {
    code("""
        @pure def x[A <: Int](f: String => A): Int = f("Hello")
        @pure def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }
  @Test def funWithPureReturnTypeAllowsPureFunction3 {
    code("""
        @pure def x[A <: Int](f: String => A): Int = f("Hello")
        def len(x: String) = x.length
        @pure def p = x(len)
        """) should compile
  }

  @Test def funWithPureReturnTypeDoesNotAllowImpureFunction {
    code("""
        @pure def x[A <: Int](f: String => A): Int = f("Hello")
        @impure def len(x: String) = x.length
        @pure def p = x[Int](len)
        """) should yieldCompileError("type mismatch")
  }
  @Test def funWithPureReturnTypeDoesNotAllowImpureFunction2 {
  	code("""
  			@pure def x[A <: Int](f: String => A): Int = f("Hello")
  			@impure def len(x: String) = x.length
  			@pure def p = x[Int @sideEffect](len)
  		""") should yieldCompileError("type mismatch")
  }
  
	@Test def canAssignSideEffectFreeToPure {
		code("""
				class A[X <: Int](val f: String => X) {}
				@pure def len(a: String) = 10
				@pure def p = new A(len)
			""") should compile
	}
	
  @Test def cannotAssignSideEffectToPure {
    code("""
        class A[X <: Int](val f: String => X) {}
        @impure def len(a: String) = 10
        @pure def p = new A(len)
        """) should yieldCompileError("type mismatch")
  }
  @Test def cannotAssignSideEffectToPure2 {
  	code("""
  			class A[X <: Int](val f: String => X) {}
  			@impure def len(a: String) = 10
  			@pure def p = new A[Int @sideEffect](len)
  		""") should yieldCompileError("type mismatch")
  }
  
  @Test def cannotAssignSideEffectFunToPureFun {
    code("""
        val f: String => Int @sideEffect = (v: String) => v.length
        val g: String => Int = f
        """) should yieldCompileError("type mismatch")
  }
  
  @Test def canAssignPureToSideEffect{
  	code("""
  	    val a: String => Int = (v: String) => v.length
  	    val f: String => Int @sideEffect = a
  			""") should compile
  }
}