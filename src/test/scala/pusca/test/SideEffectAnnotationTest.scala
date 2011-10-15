package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class SideEffectAnnotationTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def canReturnIntWhenDefinedIntSideEffect {
    code("def a: Int @sideEffect = 10") should compile
  }

  @Test def cannotReturnIntSideEffectWhenDefinedInt {
    code("""
        def a: Int @sideEffect = 10
        def b: Int = a""") should yieldCompileError("type mismatch")
  }

  @Test def methodNotSideEffectOverrideSideEffect {
    code("""
        trait A { def a: Int @sideEffect }
        trait B extends A { override def a: Int = 10 }""") should compile
  }

  @Test def methodWithSideEffectCannotOverrideMethodWithout {
    code("""
        trait A { def a: Int }
        trait B extends A { override def a: Int @sideEffect }""") should yieldCompileError("overriding method a in trait A")
  }

  @Test def sideEffectIsPreserved {
    code("""
        def a: Int @sideEffect = 10
        def b = a""") should compile
    code("""
        def a: Int @sideEffect = 10
        def b = a
        def c: Int = b""") should yieldCompileError("type mismatch")
  }

}