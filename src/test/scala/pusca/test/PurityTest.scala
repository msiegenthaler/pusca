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
    e.head should include("Impure function call to a inside the pure function b")
  }

  @Test def defImpureFunctionThatCallsAnotherImpureFunction {
    PluginTester("""
    		@impure def a(v: String) = v
    		@impure def b = a("Hi")
    	""") should be('compiled)
  }

  @Test def pureFunctionMayAccessVals {
    PluginTester("""
    		val factor = 3
    		def applyFactor(i: Int) = i * factor""") should be('compiled)
  }

  @Test def pureFunctionMayNotAccessVars {
    val e = PluginTester("""
    		var a = 0
    		def number = a""").compileErrors
    e should have size (1)
    e.head should include("Impure function call")
  }

  @Test def pureFunctionMayNotWriteVars {
    val e = PluginTester("""
  			var a = 0
  			def setA(v: Int) { a = v }""").compileErrors
    e should have size (1)
    e.head should include("Impure function call")
  }

  @Test def impureFunctionMayAccessVars {
    PluginTester("""
    		var a = 0
    		@impure def number = a""") should be('compiled)
  }

  @Test def impureFunctionMayWriteVars {
    PluginTester("""
    		var a = 0
    		@impure def addA { a = a + 1 }""") should be('compiled)
  }

  @Test def pureFunctionMayNotAccessVarsInOtherClasses {
    val e = PluginTester("""
    		object A { var a = 0 }
    		object B { def number = A.a }""").compileErrors
    e should have size (1)
    e.head should include("Impure function call")
  }

  @Test def pureFunctionMayNotWriteVarsInOtherClasses {
    val e = PluginTester("""
  			object A { var a = 0 }
  			object B { def setA(v: Int) { A.a = v } }""").compileErrors
    e should have size (1)
    e.head should include("Impure function call")
  }

  @Test def impureFunctionMayAccessVarsInOtherClasses {
    PluginTester("""
    		object A { var a = 0 }
    		object B { @impure def number = A.a }""") should be('compiled)
  }

  @Test def impureFunctionMayWriteVarsInOtherClasses {
    PluginTester("""
    		object A { var a = 0 }
    		object B { @impure def addA { A.a = A.a + 1 } }""") should be('compiled)
  }

  @Test def declarePureMakesImpurePure {
    PluginTester("""
    		@impure def a = 10
    		@declarePure def b = a
    		def c = b""") should be('compiled)
  }

}