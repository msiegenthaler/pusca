package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

/** Tests that don't work yet because of a problem of the java compiler with Pure[A] (not always the same as A @sideEffectFree)  */
class FunctionTypeTestWithPure extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def anonFunctionWithoutSideEffectIsPure4 {
    code("""
        val f: String => Pure[Int] = s => s.length
        """) should compile
  }
  @Test def anonFunctionWithoutSideEffectIsPure5 {
    code("""
        @pure def len(s: String): Pure[Int] = s.length
        val f: String => Int @sideEffectFree = s => len(s)
        """) should compile
  }
  @Test def anonFunctionWithoutSideEffectIsPure6 {
    code("""
        @pure def len(s: String): Int @sideEffectFree = s.length
        val f: String => Int @sideEffectFree = s => len(s)
        """) should compile
  }
}