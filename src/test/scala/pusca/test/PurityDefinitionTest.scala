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
    val a = "@pure def checker = " + call
    val b = "@impure def impureChecker = " + call
    new PluginTester().fromString(definition).run should compile
    new PluginTester().fromString(definition).fromString(a).fromString(b).run should
      yieldCompileError("method 'checker' has @pure annotation and a @sideEffect return type")
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
  @Test def defDeclarePureOnImpure {
    val w = PluginTester("""
	    @impure def x(a: Int) = a
	    @declarePure def dp(a: Int) = x(a)""").warnings
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
			  @impure class X { val a = ip("Hi") }""", "new X()")
  }
  @Test def impureClassConstructor2 {
    assertImpure("""@impure class X { val a = 1 }""", "new X()")
  }
  @Test def impureClassConstructorWithArguments {
    assertImpure("""
		def ip(a: String): Unit @sideEffect = ()
		@impure class X(b: Int, val c: Int) { val a = ip("Hi") }""", "new X(1, 2)")
  }

  @Test def conflictingAnnotationsYieldError {
    code("@pure def a: Int @sideEffect = 10") should
      yieldCompileError("method 'a' has @pure annotation and a @sideEffect return type")
  }

  @Test def impureImpliesSideEffectReturnType {
    code("@impure def a: Int = 10") should compile
    code("@impure def a: Int @sideEffect = 10") should compile
    code("""
        @impure def a: Int = 10
        def b = a
        @pure def c = b""") should yieldCompileError("method 'c' has @pure annotation and a @sideEffect return type")
  }
}