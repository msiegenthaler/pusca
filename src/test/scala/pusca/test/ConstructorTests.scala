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
    		def makeA = new A""") should
      yieldCompileError("impure function call inside the pure function 'makeA'")
  }

  @Test def cannotExtendClassWithImpureConstructorWithAPureOne {
    code("""
        @impure def ip(s: String) = ()
  			@impure class A { ip("hi") }
  			@pure class B extends A""") should
      yieldCompileError("impure function call inside the pure function '<init>'")
  }

  @Test def constructorNotDeclaredImpureMayNotUseVars {
    code("trait A { var a = 1 }") should yieldCompileError("access to non-local var inside the pure function '<init>'")
  }

  @Test def constructorNotDeclaredImpureMayNotCallImpureFunction {
    code("""
        @impure def ip(s: Strint) = ()
 				trait A { ip("Hi") }
        """) should yieldCompileError("impure function call inside the pure function '<init>'")
  }
}