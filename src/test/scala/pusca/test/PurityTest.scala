package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test

class PurityTest extends JUnitSuite with ShouldMatchersForJUnit {

  @Test def defPureFunctionDouble {
    PluginTester("def double(x: Int) = x * 2") should be('compiled)
  }

  @Test def defPureFunctionSum {
    PluginTester("def sum(a: Int, b: Int) = a + b") should be('compiled)
  }

  @Test def defPureFunctionSumOfList {
    PluginTester("def sum(e: List[Int]) = e.foldLeft(0)(_ + _)") should be('compiled)
  }

  @Test def defPureFunctionThatCallsAnotherPureFunction {
    PluginTester("""
    		def a(v: String) = v
    		def b = a("Hi")
    	""") should be('compiled)
  }

  @Test def defPureFunctionThatTriesToCallAnImpureOne {
    val e = PluginTester("""
    		@impure def a(v: String) = v
    		def b = a("Hi")
    	""").compileErrors
    e should have size (1)
    println(e.head)
    e.head should include("Impure function call to a inside the pure function b")
  }

  @Test def defImpureFunctionThatCallsAnotherImpureFunction {
    PluginTester("""
    		@impure def a(v: String) = v
    		@impure def b = a("Hi")
    	""") should be('compiled)
  }

}