package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class PurityDefinitionTest extends JUnitSuite with ShouldMatchersForJUnit {
  private def assertPure(definition: String, call: String) = {
    val a = "@pure def checker = " + call
    new PluginTester().fromString(definition).fromString(a).run should compile
  }
  private def assertImpure(definition: String, call: String) = {
    val a = "@pure def checker = {" + call + "; () }"
    val b = "@impure def impureChecker = { " + call + "; () }"
    val n = call.takeWhile(_ != '(')
    new PluginTester().fromString(definition).run should compile
    new PluginTester().fromString(definition).fromString(a).fromString(b).run should
      yieldCompileError("impure method call to " + n + " inside the pure method checker")
  }
  
  @Test def pureAndImpure {
    code("@pure @impure def a = 10") should yieldCompileError("method a has multiple purity annotations")
  }
  @Test def pureAndImpureIf {
    code("@pure @impureIf('A) def a = 10") should yieldCompileError("method a has multiple purity annotations")
  }
  @Test def impureAndImpureIf {
    code("@impure @impureIf('A) def a = 10") should yieldCompileError("method a has multiple purity annotations")
  }
  @Test def pureAndDeclarePure {
    code("@pure @declarePure def a = 10") should yieldCompileError("method a has multiple purity annotations")
  }
  @Test def impureAndDeclarePure {
    code("@impure @declarePure def a = 10") should yieldCompileError("method a has multiple purity annotations")
  }
  @Test def impureIfAndDeclarePure {
    code("@impureIf @declarePure def a = 10") should yieldCompileError("method a has multiple purity annotations")
  }
  
  @Test def cannotUseSideEffectOnMethod {
    code("@sideEffect def a = 10") should yieldCompileError("@sideEffect does not apply to methods but only to types")
  }
  
  @Test def cannotUsePureOnReturnType {
    code("def a: Int @pure = 10") should yieldCompileError("purity annotation @pure can only be used on a method, not on the return type")
  }
  @Test def cannotUseImpureOnReturnType {
  	code("def a: Int @impure = 10") should yieldCompileError("purity annotation @impure can only be used on a method, not on the return type")
  }
  @Test def cannotUseDeclarePureOnReturnType {
  	code("def a: Int @declarePure = 10") should yieldCompileError("purity annotation @declarePure can only be used on a method, not on the return type")
  }
  @Test def cannotUseImpureIfOnReturnType {
  	code("def a: Int @impureIf('A) = 10") should yieldCompileError("purity annotation @impureIf can only be used on a method, not on the return type")
  }

  @Test def defPureFunctionImplicit {
    assertPure("def p(x: Int) = x * 2", "p(0)")
  }
  @Test def defPureFunctionExplicit {
    assertPure("@pure def p(x: Int) = x * 2", "p(0)")
  }

  @Test def defImpureFunction {
    assertImpure("@impure def ip(a: Int) = 10", "ip(10)")
  }
  @Test def defImpureFunctionSideEffectUnit {
    assertImpure("def ip(a: Int): Unit @sideEffect = ()", "ip(10)")
  }
  @Test def defImpureFunctionSideEffectInt {
    assertImpure("def ip(a: Int): Int @sideEffect = 10", "ip(20)")
  }

  @Test def defImpureFunctionImplicitThroughLastStatement {
    assertImpure("""
  		def ip(a: String): Unit @sideEffect = ()
  	  def ip2(a: String) = ip(a)""", "ip2(\"Hi\")")
  }
  @Test def defImpureFunctionImplicitThroughLastStatement2 {
    assertImpure("""
    		@impure def ip(a: String): Unit = ()
    		def ip2(a: String) = ip(a)""", "ip2(\"Hi\")")
  }

  @Test def defImpureFunctionExplicitAndThroughLastStatement {
    assertImpure("""
  		@impure def ip(a: String): Unit = ()
  	  @impure def ip2(a: String) = ip(a)""", "ip2(\"Hi\")")
  }

  @Test def defDeclarePureOnPure {
    val w = PluginTester("@declarePure def dp(a: Int) = a").warnings
    w should have size (1)
    w.head should include("@declarePure is unnecessary, function 'dp' is pure")
  }
  @Test def defDeclarePureOnPureMiddle {
    val w = PluginTester("""
        @declarePure def dp(a: Int) = {
        	a
        	10
    		}""").warnings
    w should have size (1)
    w.head should include("@declarePure is unnecessary, function 'dp' is pure")
  }
  @Test def defDeclarePureOnImpure {
    val w = PluginTester("""
	    @impure def x(a: Int) = a
	    @declarePure def dp(a: Int) = x(a)""").warnings
    w should have size (0)
  }
  @Test def defDeclarePureOnImpureMiddle {
    val w = PluginTester("""
	    @impure def x(a: Int) = a
	    @declarePure def dp(a: Int) = { 
        x(a)
        10
    	}""").warnings
    w should have size (0)
  }

  @Test def pureClassConstructor {
    assertPure("class X { val a = 0 }", "new X()")
  }
  @Test def pureClassConstructorWithArguments {
    assertPure("class X(a: Int, val b: Int) { val x = 0 }", "new X(1, 2)")
  }
  @Test def impureClassConstructor {
    assertImpure("""
			  def ip(a: String): Unit @sideEffect = ()
			  @impure class X { val a = ip("Hi") }
        @impure def run = new X()""", "run")
  }
  @Test def impureClassConstructor2 {
    code("""
        @impure class X { val a = 1 }
        @pure def checker = {
        	new X()
        	()
    		}
    		@impure def impureChecker = {
        	new X()
    			()
    		}""") should yieldCompileError("impure method call to X.<init> inside the pure method checker")
  }
  @Test def impureClassConstructorWithArguments {
    assertImpure("""
    		def ip(a: String): Unit @sideEffect = ()
    		@impure class X(b: Int, val c: Int) { val a = ip("Hi") }
    		@impure def run = new X(1, 2)""", "run")
  }

  @Test def conflictingAnnotationsYieldError {
    code("@pure def a: Int @sideEffect = 10") should
      yieldCompileError("method a has @pure annotation and a @sideEffect return type")
  }

  @Test def impureImpliesSideEffectReturnType {
    code("@impure def a: Int = 10") should compile
    code("@impure def a: Int @sideEffect = 10") should compile
    code("""
        @impure def a: Int = 10
        def b = a
        @pure def c = b""") should yieldCompileError("impure method call to b inside the pure method c")
  }
}