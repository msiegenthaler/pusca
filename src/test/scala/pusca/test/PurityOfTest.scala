package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class PurityOfTest extends JUnitSuite with ShouldMatchersForJUnit {
  private def code(c: String) = {
    val p = """
      def assertPure(purity: Purity) = if (purity != AlwaysPure) throw new AssertionError("expected AlwaysPure but was "+purity) 
      def assertImpure(purity: Purity) = if (purity != AlwaysImpure) throw new AssertionError("expected AlwaysImpure but was "+purity) 
      """
    PluginTester.code(p + "\n" + c)
  }

  @Test def purityOfIntIsAlwaysPure {
    code("assertPure(purityOf(10))") should compileAndRun
  }

  @Test def purityOfStringIsAlwaysPure {
    code("assertPure(purityOf(\"Hello\"))") should compileAndRun
  }

  @Test def purityOfSimpleValIsAlwaysPure {
    code("""
        val a = 100
        assertPure(purityOf(a))
        """) should compileAndRun
  }

  @Test def purityOfPureNoArgMethodIsAlwaysPure {
    code("""
        @pure def pm = 10
        assertPure(purityOf(pm))""") should compileAndRun
  }
  @Test def purityOfPureSingleArgMethodIsAlwaysPure {
    code("""
        @pure def d(nr: Int) = nr * 2
        assertPure(purityOf(d(10)))""") should compileAndRun
  }

  @Test def purityOfImpurePureNoArgMethodIsAlwaysImpure {
    code("""
        @impure def im = 10
        assertImpure(purityOf(im))""") should compileAndRun
  }
  @Test def purityOfImpurePureSingleArgMethodIsAlwaysImpure {
    code("""
        @impure def im(a: Int) = 10 * a
        assertImpure(purityOf(im(2)))""") should compileAndRun
  }

  //TODO make literals sideEffectFree
  @Test def purityOfParametrizedToStringIsAlwaysImpure {
    code("""
        def d[A](in: A): A = in
        assertImpure(purityOf(d("Hi")))""") should compileAndRun
  }
  //TODO make literals sideEffectFree
  @Test def purityOfParametrizedToIntIsAlwaysImpure {
    code("""
        def d[A](in: A): A = in
        assertImpure(purityOf(d(10)))""") should compileAndRun
  }

  @Test def purityOfParametrizedToSefStringIsAlwaysPure {
    code("""
        def d[A](in: A): A @sideEffectFree = in
        assertPure(purityOf(d("Hi")))""") should compileAndRun
  }
  @Test def purityOfParametrizedToSefIntIsAlwaysPure {
    code("""
        def d[A](in: A): A @sideEffectFree = in
        assertPure(purityOf(d(10)))""") should compileAndRun
  }

  @Test def purityOfParametrizedViaInferenceToSefStringIsAlwaysPure {
    code("""
        def a: String @sideEffectFree = "hi"
        def d[A](in: A): A = in
        assertPure(purityOf(d[String @sideEffectFree](a)))""") should compileAndRun
  }

}