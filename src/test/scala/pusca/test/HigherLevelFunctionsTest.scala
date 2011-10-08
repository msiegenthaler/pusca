package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class HigherLevelFunctionsTest extends JUnitSuite with ShouldMatchersForJUnit {

  @Test def functionWithSideEffectIsNotCompatibleWithPure {
    code("""
        @pure def m(f: String => Int) = {
        	f("Hi) == 0
    		}
        @impure def p(s: String) = s.length
    		@pure def x = {
        	m(p)
    		}
      """) should yieldCompileError("impure function call inside the pure function x")
  }
  
  @Test def functionWithSideEffectIsNotCompatibleWithPurePartialApplication {
  	code("""
  			@pure def m(f: String => Int) = {
  				f("Hi) == 0
  			}
  			@impure def p(i: Int)(s: String) = s.length * i
  			@pure def x = {
  				m(p(2))
  			}
  	""") should yieldCompileError("impure function call inside the pure function x")
  }
  @Test def functionWithSideEffectIsNotCompatibleWithPurePartialApplication2 {
  	code("""
  			@pure def m(f: String => Int) = {
  				f("Hi) == 0
  			}
  			@impure def p(i: Int, s: String) = s.length * i
  			@pure def x = {
  				m(p(2, _))
  			}
  	""") should yieldCompileError("impure function call inside the pure function x")
  }

  @Test def functionWithSideEffectCannotBeEvaluatedInsidePureFunction {
    code("""
        @pure def m(f: String => Unit @sideEffect): Unit = f("Hi")
      """) should yieldCompileError("impure function call inside the pure function m")
  }
  @Test def functionWithSideEffectCanBePassedToPureFunction {
    code("""
        @pure def m(f: String => Unit @sideEffect): String = "Hi"
      """) should compile
  }
  
  @Test def functionWithSideEffectIsCompatibleWithImpure {
    code("""
        @impure def m(f: String => Int @sideEffect) = f("Hi")
        @impure def l(f: String) = f.length
        @impure def x = m(l)
      """) should compile
  }
  @Test def functionWithSideEffectIsCompatibleWithPure {
  	code("""
  			@impure def m(f: String => Int @sideEffect) = f("Hi")
  			@pure def l(f: String) = f.length
  			@impure def x = m(l)
  	""") should compile
  }

  @Test def functionWithTypeParameterIncludesPurenessOnPureWithClass {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  	    class LengthFun extends Function1[String,Int] { override def apply(s: String) = s.length }
  			@pure def x = m(new LengthFun)
  		""") should compile
  }
  @Test def functionWithTypeParameterIncludesPurenessOnPureWithAnon {
    code("""
        	def m[A](f: String => A): A = f("Hello")
          m(_ + "!")
        """) should compile
  }
  @Test def functionWithTypeParameterIncludesPurenessOnPureWithAnon2 {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@pure def x = m(_.length)
  		""") should compile
  }
  @Test def functionWithTypeParameterIncludesPurenessOnPureWithAnonLong {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@pure def x = m(x => x.length)
  		""") should compile
  }

  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithClass {
    code("""
  			def m[A](f: String => A): A = f("Hello")
        @impure def il(i: Int) = i + i
  	    class LengthFunI extends Function1[String,Int @sideEffect] { override def apply(s: String) = il(s.length) }
  			@pure def x = m(new LengthFunI)
  		""") should yieldCompileError("impure function call inside the pure function 'x'")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithClass2 {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: Int) = i + i
  			class LengthFunI extends Function1[String,Int @sideEffect] { override def apply(s: String) = il(s.length) }
  			@pure def x = {
  	    	m(new LengthFunI)
  	    	"Huhu"
  			}
  		""") should yieldCompileError("impure function call inside the pure function 'x'")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnon {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = m(il)
  		""") should yieldCompileError("impure function call inside the pure function 'x'")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonAndUnit {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = ()
  			@pure def x = m(il)
  		""") should yieldCompileError("impure function call inside the pure function 'x'")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonLong {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = m(s => il(s))
  		""") should yieldCompileError("impure function call inside the pure function 'x'")
  }
}