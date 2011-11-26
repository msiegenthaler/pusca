package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class PureFunctionTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def pureFunction1DefOk {
    code("val a: String -> Int = (a: String) => a.length") should compile
  }
  @Test def pureFunction1DefSideEffectImplPure {
    code("val a: String -> (Int @sideEffect) = (a: String) => a.length") should yieldCompileError("PureFunction mustn't have a side effect")
  }
  @Test def pureFunction1DefSideEffectImplPure2 {
    code("val a: String -> Int @sideEffect = (a: String) => a.length") should yieldCompileError("PureFunction mustn't have a side effect")
  }
  @Test def pureFunction1DefSideEffectAndImplSideEffect {
    code("val a: String -> (Int @sideEffect) = (a: String) => addSideEffect(a.length)") should yieldCompileError("type mismatch")
  }
  @Test def pureFunction1DefSideEffectAndImplSideEffect2 {
    code("val a: String -> Int @sideEffect = (a: String) => addSideEffect(a.length)") should yieldCompileError("type mismatch")
  }

  @Test def pureFunction1DefFromPureFun {
    code("""
        def p(a: String) = a.length
        val a: String -> Int = p _
        """) should compile
  }
  @Test def pureFunction1SideEffectDefFromPureFun {
    code("""
        def p(a: String) = a.length
        val a: String -> (Int @sideEffect) = p _
        """) should yieldCompileError("PureFunction mustn't have a side effect")
  }
  @Test def pureFunction1SideEffectDefFromPureFun2 {
  	code("""
  			def p(a: String) = a.length
  			val a: String -> Int @sideEffect = p _
  	""") should yieldCompileError("PureFunction mustn't have a side effect")
  }

  @Test def cannotAssignImpureFunctionToPure {
    code("""
        @impure def ip(a: String) = a.length
        val a: String -> Int = ip _
        """) should yieldCompileError("type mismatch")
  }

  @Test def cannotAssignImpureFunctionToPure2 {
    code("""
        @impure def ip(a: String) = a.length
        val a = ip _
        val b: String -> Int = a
        """) should yieldCompileError("type mismatch")
  }

  @Test def cannotAssignImpureFunctionToPure3 {
    code("""
  			@impure def ip(a: String) = a.length
  			val a: String => Int @sideEffect = ip _
  			val b: String -> Int = a
  	""") should yieldCompileError("type mismatch")
  }

  @Test def cannotAssignImpureFunctionToPure4 {
    code("""
  			@impure def ip(a: String) = a.length
  			val a: Function1[String, Int @sideEffect] = ip _
  			val b: String -> Int = a
  	""") should yieldCompileError("type mismatch")
  }
}