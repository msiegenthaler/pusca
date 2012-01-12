package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class PurityOfTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def purityOfIntIsAlwaysPure {
    code("assert(purityOf(10) == AlwaysPure)") should compileAndRun
  }

  @Test def purityOfStringIsAlwaysPure {
    code("assert(purityOf(\"Hello\") == AlwaysPure)") should compileAndRun
  }

  @Test def purityOfSimpleValIsAlwaysPure {
    code("""
        val a = 100
        assert(purityOf(a) == AlwaysPure)
        """) should compileAndRun
  }

  @Test def purityOfPureNoArgMethodIsAlwaysPure {
    code("""
        @pure def pm = 10
        assert(purityOf(pm) == AlwaysPure)""") should compileAndRun
  }
  @Test def purityOfPureSingleArgMethodIsAlwaysPure {
    code("""
        @pure def d(nr: Int) = nr * 2
        assert(purityOf(d(10)) == AlwaysPure)""") should compileAndRun
  }


  @Test def purityOfImpurePureNoArgMethodIsAlwaysImpure {
    code("""
        @impure def im = 10
        assert(purityOf(im) == AlwaysImpure)""") should compileAndRun
  }
  @Test def purityOfImpurePureSingleArgMethodIsAlwaysImpure {
    code("""
        @impure def im(a: Int) = 10 * a
        assert(purityOf(im(2)) == AlwaysImpure)""") should compileAndRun
  }
}