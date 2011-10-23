package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class ConstructorTests extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def pureFunctionMayCallPureConstructor {
    code("""
    		@impure class A { val x = "Hi" }
    		def makeA = new A""") should compile
  }

  @Test def pureFunctionMayCallPureConstructor2 {
    code("""
  			@impure class A(a: String)
  			def makeA = new A("Hi")""") should compile
  }

  @Test def impureFunctionMayCallImpureConstructor {
    code("""
  			@impure class A(var x: String)
  			@impure def makeA = new A("hi")""") should compile
  }

  @Test def impureFunctionMayCallPureConstructor {
    code("""
  			@impure class A { val x = "Hi" }
  			@impure def makeA = new A""") should compile
  }

  @Test def pureFunctionCannotCallImpureConstuctor {
    code("""
    		@impure class A { var x = "Hi" }
    		def makeA = {
        	new A
        	()
    		}""") should
      yieldCompileError("impure method call inside the pure method 'makeA'")
  }
  
  @Test def pureFunctionCannotCallImpureConstuctor2 {
  	code("""
  			@impure class A { var x = "Hi" }
  			@pure def makeA = new A
  		""") should
  	yieldCompileError("method 'makeA' has @pure annotation and a @sideEffect return type")
  }
  
  @Test def callingImpureNewLastMakesAMethodImpure {
  	code("""
  			@impure class A { var x = "Hi" }
  			def makeA = new A
  	    @pure def run = makeA
  		""") should
  	yieldCompileError("method 'run' has @pure annotation and a @sideEffect return type")
  }

  @Test def cannotExtendClassWithImpureConstructorWithAPureOne {
    code("""
        @impure def ip(s: String) = ()
  			@impure class A { ip("hi") }
  			@pure class B extends A""") should
      yieldCompileError("impure function call inside the pure function '<init>'")
  }

  @Test def constructorNotDeclaredImpureMayNotUseVars {
    code("""trait X {
        			var a = 10
        			trait A { val b = a }
    				}""") should yieldCompileError("access to non-local var inside the pure class 'X.A'")
  }

  @Test def constructorNotDeclaredImpureMayNotCallImpureFunction {
    code("""
        @impure def ip(s: String) = ()
 				trait A { ip("Hi") }
        """) should yieldCompileError("impure method call inside the pure class 'A'")
  }
}