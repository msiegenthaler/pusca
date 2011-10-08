package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._


class ScalaLibraryCallsTest extends JUnitSuite with ShouldMatchersForJUnit {

  @Test def callToExternalImpureFunctionPrintlnInsidePureFunction {
    code("def b(a: String) = println(a)") should
    	yieldCompileError("impure function call inside the pure function 'b'")
  }

  @Test def callToExternalImpureFunctionPrintlnInsideImpureFunction {
    code("@impure def b(a: String) = println(a)") should compile
  }
}