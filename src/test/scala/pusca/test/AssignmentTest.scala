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
}