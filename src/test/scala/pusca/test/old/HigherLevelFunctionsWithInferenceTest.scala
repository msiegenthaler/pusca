package pusca.test.old

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class HigherLevelFunctionsWithInferenceTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithVal {
  	code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = {
  	    	val f: Function[String,Int @sideEffect] = il _
  	    	val r = m(f)
  	    	r
  			}
  	""") should yieldCompileError("method 'x' has @pure annotation and a @sideEffect return type")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithInferedVal {
  	code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = {
  	    	val f = il _
  	    	m(f)
  			}
  	""") should yieldCompileError("method 'x' has @pure annotation and a @sideEffect return type")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnon {
  	code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = m(il _)
  	""") should yieldCompileError("method 'x' has @pure annotation and a @sideEffect return type")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonMiddle {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = {
        	m(x => il(x))
        	10
    		}
  		""") should yieldCompileError("impure method call inside the pure method 'x'")
  }

  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonAndUnit {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = ()
  			@pure def x = m(il _)
  		""") should yieldCompileError(" method 'x' has @pure annotation and a @sideEffect return type")
  }

  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonAndUnitMiddle {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = ()
  			@pure def x = {
  				m(il _)
  				10
  			}
  		""") should yieldCompileError(" method 'x' has @pure annotation and a @sideEffect return type")
  }
  
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonLong {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = m(s => il(s))
  		""") should yieldCompileError("impure method call inside the pure method 'x'")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonLong2 {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			def il(i: String): Int @sideEffect = i.length
  			@pure def x = m(s => il(s))
  		""") should yieldCompileError("impure method call inside the pure method 'x'")
  }

  @Test def pureFunctionWithParameterThatIsNotAReturnValueDoesNotAcceptImpure {
  	code("""
  			@pure def m[A](f: String => A): Unit = {
  				f("Huhu")
  				()
  			}
  	    @impure def l(s: String) = s.length
  			@pure def x = m(l _)
  	""") should yieldCompileError("type-parameter of function m must not be impure")
  }
  
  @Test def pureFunctionWithParameterDeclaredSideEffectAcceptsImpure {
    code("""
        @pure def m[A](f: String => A): Unit = ()
    		@impure def l(s: String) = s.length
        @pure def x = m(l _)
      """) should compile
  }

  @Test def methodWithOptionGetOrElseHasSideEffectIfGetOrElseHas {
    code("""
        @impure def ip = "Hello"
        def m(o: Option[String]) = {
    		o.getOrElse(ip)
    	}
        @pure def p = m(Some("Hi"))
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
}