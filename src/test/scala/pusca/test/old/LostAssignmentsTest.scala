package pusca.test.old

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class LostAssignmentsTest extends JUnitSuite with ShouldMatchersForJUnit {

  @Test def warnOnNotAssignedLiteral {
    code("""
    		def a = {
    			"Hello"
    			"Huhu"
    		}""") should warn("literal is not used")
  }

  @Test def dontWarnIfAssigned {
    code("""
  			def a = {
  				val a = "Hello"
    			"huhu"
  			}""") should compile
  }

  @Test def warnOnLastStatementInUnitMethods {
    code("""
  			def a {
  				"Hello"
  			}""") should warn("literal is not used")
  }

  @Test def warnOnLastStatementInUnitMethods2 {
    code("""
  			def a: Unit = {
  				"Hello"
  			}""") should warn("literal is not used")
  }

  @Test def dontWarnOnLastStatementInMethods {
    code("""
  			def a: String = {
  				"Hello"
  			}""") should compile
  }

  @Test def dontWarnOnLastStatementInMethods2 {
    code("""
  			def a = {
  				"Hello"
  			}""") should compile
  }

  @Test def warnOnNotAssignedMethodResult {
    code("""
    		def b = 200
    		def a = {
    			b
    			10
    		}""") should warn("result of call is not used")
  }

  @Test def warnOnPointlessCallsInConstructor {
    code("""
    		def b = 200
    		class A {
    			b
    		}""") should warn("result of call is not used")
  }

  @Test def dontWarnOnCallsInConstructorThatAreAssignedToVal {
    code("""
  			def b = 200
  			class A {
  				val a = b
  			}""") should compile
  }

  @Test def dontWarnOnImpureCalls {
    code("""
  			@impure def a = "Hallo"
  			@impure def b = {
  				a
  				10
  			}""") should compile
  }

  @Test def warnOnLiteralInImpure {
    code("""
  			@impure def b = {
  				20
  				10
  			}""") should warn("literal is not used")
  }

  @Test def warnOnPureFunctionInImpure {
    code("""
  			def a = 20
  			@impure def b = {
  				a
  				10
  			}""") should warn("result of call is not used")
  }
}