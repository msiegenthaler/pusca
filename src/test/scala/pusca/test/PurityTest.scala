package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import PluginTester._

class PurityTest extends JUnitSuite with ShouldMatchersForJUnit {

  def code(s: String) = {
    new PluginTester().fromString("@impure def ip(i: Int) = 10").fromString(s).run
  }

  @Test def pureFunctionMustNotCallImpureFunction {
    code("@pure def a = ip(10)") should yieldCompileError("impure function call inside the pure function 'a'")
  }

  @Test def pureFunctionMayCallAPureFunctions {
    code("""
        @pure def a(i: Int) = i + 10
        @pure def b(i: Int) = a(i)
      """) should compile
  }

  @Test def pureFunctionMayCallMultipleOtherPureFunctionsAndUseVals {
    code("""
        @pure def sum(a: Int, b: Int) = a + b
    		@pure def square(i: Int) = i * i
        @pure def b(i: Int) = {
        	val sq = square(i)
        	val s = sum(i, sq)
        	s
        }
      """) should compile
  }

  @Test def pureFunctionMayHaveInnerPureFunction {
    code("""
        	@pure def a(i: Int) = {
		        def sum(a: Int, b: Int) = a + b
		    		def square(i: Int) = i * i
        		sum(square(i), i)
    			}
        """) should compile
  }

  @Test def pureFunctionMustNotAccessVars {
    code("""
  	     class A {
  	       var x: Int = 0
  			   @pure def a = x
         }
  	  """) should yieldCompileError("access to non-local var inside the pure function 'a'")
  }

  @Test def pureFunctionMustNotModifyVars {
    code("""
  			 class A {
  			  var x: Int = 0
  			  @pure def a(i: Int) { x = i }
  			 }
  	  """) should yieldCompileError("write to non-local var inside the pure function 'a'")
  }

  @Test def pureFunctionMustNotAccessVarInOtherClasses {
    code("""
        object A { var a = 0 }
        class B {
        	@pure def x = A.a + 10
        }
      """) should yieldCompileError("access to non-local var inside the pure function 'x'")
  }

  @Test def pureFunctionMustNotWriteVarInOtherClasses {
    code("""
        object A { var a = 0 }
        class B {
        	@pure def x = {
        		A.a = A.a + 1
        		A.a
    			}
        }
      """) should yieldCompileError("write to non-local var inside the pure function 'x'")
  }

  @Test def pureFunctionMayAccessVals {
    code("""
        class A {
        	val x = 1021
        	@pure def a = x
    		}
      """) should compile
  }

  @Test def pureFunctionMayAccessLocalVars {
    code("""
        @pure def a(i: Int) = {
        	var x = i
        	x = i + 10
        	x
        }
      """) should compile
  }

  @Test def pureFunctionMayNotAccessVarsOfOuterFunction {
    code("""
        @pure def a = {
          var x = 10
          @pure def b = x + 1
        	b
    	  }
      """) should yieldCompileError("access to non-local var inside the pure function 'b'")
  }

  @Test def pureFunctionMayNotWriteVarsOfOuterFunction {
    code("""
  			@pure def a = {
  			  var x = 10
  		  	@pure def b { x = x + 1 }
  		  	b
  	    	x
  			}
  	  """) should yieldCompileError("write to non-local var inside the pure function 'b'")
  }
}