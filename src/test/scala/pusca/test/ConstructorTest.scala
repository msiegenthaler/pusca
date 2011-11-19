package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class ConstructorTest extends JUnitSuite with ShouldMatchersForJUnit {
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
      yieldCompileError("impure method call to A.<init> inside the pure method makeA")
  }
  @Test def pureFunctionCannotCallImpureConstuctor2 {
    code("""
  			@impure class A { var x = "Hi" }
  			@pure def makeA = new A
  		""") should yieldCompileError("impure method call to A.<init> inside the pure method makeA")
  }

  @Test def callingImpureNewLastMakesAMethodImpure {
    code("""
  			@impure class A { var x = "Hi" }
  			def makeA = new A
  	    @pure def run = makeA
  		""") should yieldCompileError("impure method call to makeA inside the pure method run")
  }

  @Test def constructorNotDeclaredImpureMayNotUseVars {
    code("""trait X {
        			var a = 10
        			trait A { val b = a }
    				}""") should yieldCompileError("access to non-local var a inside the pure class X.A")
  }

  @Test def constructorNotDeclaredImpureMayNotCallImpureFunction {
    code("""
        @impure def ip(s: String) = ()
 				trait A { ip("Hi") }
        """) should yieldCompileError("impure method call to ip inside the pure class A")
  }

  @Test def impureConstructorMayCallImpureMethods {
    code("""
        @impure def ip(s: String) = ()
 				@impure trait A { ip("Hi") }
        """) should compile
  }

  @Test def impureConstructorMayCallImpureMethodsWithVal {
    code("""
        @impure def ip(s: String) = ()
 				@impure trait A { val x = ip("Hi") }
        """) should compile
  }

  @Test def impureConstructorMayAccessVars {
    code("""
        @impure def ip(s: String) = ()
 				object A { var x = 10 }
        @impure class B {
        	val b = A.x
    		}
        """) should compile
  }

  @Test def constructorDeclaredImpureMayUseVars {
    code("""trait X {
        			var a = 10
        			@impure trait A { val b = a }
    				}""") should compile
  }
}