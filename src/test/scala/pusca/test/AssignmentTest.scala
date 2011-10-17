package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class AssignmentTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def valuesDefinedSideEffectAreNotAllowed {
    code("val a: Int @sideEffect = 10") should
      yieldCompileError("declaration of val with @sideEffect is not allowed")
  }

  @Test def varsDefinedSideEffectAreNotAllowed {
    code("var a: Int @sideEffect = 10") should
      yieldCompileError("declaration of var with @sideEffect is not allowed")
  }

  @Test def sideEffectCanBeUsedOnMethodReturnType {
    code("def a(a: Int): Int @sideEffect = a") should compile
  }

  @Test def sideEffectCanBeUsedOnMethodTypeParam {
    code("@impure def a[A <: Any @sideEffect](a: A) = 10")
  }

  @Test def sideEffectCanBeUsedOnClassTypeParam {
    code("trait A[A,B <: Any @sideEffect] { def a(a: A): B }") should compile
  }

  @Test def assignImpureToPureInsideImpureWithoutType {
    code("""
        @impure def c: Int = 100
        @impure def b = {
        	val a = c
        	a + 10
    		}
        """) should compile
  }
  
  @Test def assignImpureToPureInsideImpureWithoutType2 {
    code("""
  			@impure def c(i: Int): Int = 100
  			@impure def b = {
	  			val a = c(2)
	  			a + 10
  			}
  	""") should compile
  }

  @Test def assignImpureToPureInsideImpureWithType {
    code("""
        @impure def c: Int = 100
        @impure def b = {
        	val a: Int = c
        	a + 10
    		}
        """) should compile
  }

  @Test def assignImpureToPureInsideImpureWithType2 {
    code("""
  			@impure def c(i: Int): Int = 100
  			@impure def b = {
        	val delta = 10
	  			val a: Int = c(1)
	  			a + delta
  			}
  			""") should compile
  }

  @Test def assignImpureToPureInsideImpureWithType3 {
    code("""
  			@impure def c(i: Int): Int = 100
  			@impure def b = {
        	def delta(f: Int) = 10 * f
	  			val a: Int = c(1)
	  			a + delta(2)
  			}
  			""") should compile
  }

  @Test def callFunctionWithParameterFromImpureInsideImpure {
    code("""
  			@impure def c(i: Int): Int = 100
        def twice(i: Int) = i * 2
  			@impure def b = {
        	twice(c(10))
  			}
  			""") should compile
  }

  @Test def callFunctionWithParameterFromImpureInsideImpureValWithoutType {
    code("""
  			@impure def c(i: Int): Int = 100
        def twice(i: Int) = i * 2
  			@impure def b = {
        	val x = pusca.applySideEffect(twice(c(10)))
        	x
  			}
  			""") should compile
  }
  
  @Test def callFunctionWithParameterFromImpureInsideImpureValWithType {
    code("""
  			@impure def c(i: Int): Int = 100
        def twice(i: Int) = i * 2
  			@impure def b = {
        	val x: Int = twice(c(10))
        	x
  			}
  			""") should compile
  }
  
  @Test def callFunctionWithParameterFromImpureInsideImpureVarWithType {
  	code("""
  			@impure def c(i: Int): Int = 100
  			def twice(i: Int) = i * 2
  			@impure def b = {
  			var x = twice(c(10))
  			x
  			}
  	""") should compile
  }
  
  @Test def callFunctionWithParameterFromImpureInsideImpureVarWithoutType {
    code("""
  			@impure def c(i: Int): Int = 100
        def twice(i: Int) = i * 2
  			@impure def b = {
        	var x = twice(c(10))
        	x
  			}
  			""") should compile
  }

  @Test def callFunctionWithParameterFromImpureInsideImpureWithAnnots {
    code("""
        class xxx extends StaticAnnotation
  			@impure def c(i: Int): Int = 100
  			@impure def b = {
        	val x: Int @xxx = c(10)
        	x
  			}
  			""") should compile
  }
}