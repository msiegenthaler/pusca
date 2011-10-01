package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test

class ScalaLibraryCallsTest extends JUnitSuite with ShouldMatchersForJUnit {
  
  @Test def callToExternalImpureFunctionPrintlnInsidePureFunction {
    val e = PluginTester("def b(a: String) = println(a)").compileErrors
    e should have size (1)
    println(e.head)
    e.head should include("Impure function call to scala.Predef.println inside the pure function b")
  }

  @Test def callToExternalImpureFunctionPrintlnInsideImpureFunction {
    PluginTester("@impure def b(a: String) = println(a)") should be('compiled)
  }
  
}