package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test

class LostAssignmentsTest extends JUnitSuite with ShouldMatchersForJUnit {

  @Test def warnOnNotAssignedLiteral {
    val w = PluginTester("""
    		def a = {
    			"Hello"
    			"Huhu"
    		}
    	""").problems
    w should have size (1)
    w.head should include("Literal is not used")
  }

  @Test def dontWarnIfAssigned {
    val w = PluginTester("""
  			def a = {
  				val a = "Hello"
    			"huhu"
  			}""").problems
    w should have size (0)
  }

  @Test def warnOnLastStatementInUnitMethods {
    val w = PluginTester("""
  			def a {
  				"Hello"
  			}""").problems
    w should have size (1)
    w.head should include("Literal is not used")
  }

  @Test def warnOnLastStatementInUnitMethods2 {
    val w = PluginTester("""
  			def a: Unit = {
  				"Hello"
  			}""").problems
    w should have size (1)
    w.head should include("Literal is not used")
  }

  @Test def dontWarnOnLastStatementInMethods {
    val w = PluginTester("""
  			def a: String = {
  				"Hello"
  			}""").problems
    w should have size (0)
  }

  @Test def dontWarnOnLastStatementInMethods2 {
    val w = PluginTester("""
  			def a = {
  				"Hello"
  			}""").problems
    w should have size (0)
  }

  @Test def warnOnNotAssignedMethodResult {
    val w = PluginTester("""
    		def b = 200
    		def a = {
    			b
    			10
    		}""").problems
    w should have size (1)
    w.head should include("Result of call is not used")
  }

  @Test def warnOnPointlessCallsInConstructor {
    val w = PluginTester("""
    		def b = 200
    		class A {
    			b
    		}""").problems
    w should have size (1)
    w.head should include("Result of call is not used")
  }

  @Test def dontWarnOnCallsInConstructorThatAreAssignedToVal {
    val w = PluginTester("""
  			def b = 200
  			class A {
  				val a = b
  			}""").problems
    w should have size (0)
  }
  
  @Test def dontWarnOnImpureCalls {
  	val w = PluginTester("""
  			@impure def a = "Hallo"
  			@impure def b = {
  				a
  				10
  			}""").problems
  	w should have size (0)
  }
  
  @Test def warnOnLiteralInImpure {
  	val w = PluginTester("""
  			@impure def b = {
  				20
  				10
  			}""").problems
  	w should have size (1)
  	w.head should include("Literal is not used")
  }
  
  @Test def warnOnPureFunctionInImpure {
  	val w = PluginTester("""
  			def a = 20
  			@impure def b = {
  				a
  				10
  			}""").problems
  	w should have size (1)
  	w.head should include("Result of call is not used")
  }

}