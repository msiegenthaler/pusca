package pusca.test.old

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import PluginTester._
import org.junit.Test

class ImpurityTest extends JUnitSuite with ShouldMatchersForJUnit {
  def code(s: String) = {
    new PluginTester().
      fromString("@impure def ip(i: Int) = 10").
      fromString("@pure def p(i: Int) = i").
      fromString(s).run
  }

  @Test def impureFunctionMayCallAPureFunction {
    code("@impure def a = p(10)") should compile
  }
  
  @Test def impureFunctionMayCallAnImpureFunction {
    code("@impure def a = ip(10)") should compile
  }
  
  @Test def impureFunctionMayUseLocalVars {
    code("""
        @impure def a = {
        	var x = 10
          x = x + 1
          x
        }
      """) should compile
  }
  
  @Test def impureFunctionMayAccessVars {
    code("""
  	     class A {
  	       var x: Int = 0
  			   @impure def a = x
         }
  	  """) should compile
  }

  @Test def impureFunctionMayModifyVars {
    code("""
  			 class A {
  			  var x: Int = 0
  			  @impure def a(i: Int) { x = i }
  			 }
  	  """) should compile
  }

  @Test def impureFunctionMayAccessVals {
    code("""
        class A {
        	val x = 1021
        	@impure def a = x
    		}
      """) should compile
  }
  
  @Test def impureFunctionMayAccessVarInOtherClasses {
    code("""
        object A { var a = 0 }
        class B {
        	@impure def x = A.a + 10
        }
      """) should compile
  }

  @Test def impureFunctionMayWriteVarInOtherClasses {
    code("""
        object A { var a = 0 }
        class B {
        	@impure def x = {
        		A.a = A.a + 1
        		A.a
    			}
        }
      """) should compile
  }
}