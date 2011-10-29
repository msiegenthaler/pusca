package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import PluginTester._

class PurityTest extends JUnitSuite with ShouldMatchersForJUnit {

  def code(s: String) = {
    new PluginTester().fromString("@impure def ip(i: Int): Int @sideEffect = 10\n" + s).run
  }

  @Test def pureFunctionMustNotCallImpureFunctionLast {
    code("@pure def a = ip(10)") should yieldCompileError("method 'a' has @pure annotation and a @sideEffect return type")
  }

  @Test def pureFunctionMustNotCallImpureFunctionLast2 {
    code("@pure def a: Int = ip(10)") should yieldCompileError("type mismatch")
  }

  @Test def pureFunctionMustNotCallImpureFunctionMiddleWithoutAssignment {
    code("""
  	    @pure def a = {
  	    	ip(10)
  	    	20
  			}""") should yieldCompileError("impure method call inside the pure method 'a'")
  }

  @Test def pureFunctionMustNotCallImpureFunctionMiddleWithAssignment {
    code("""
  			@pure def a = {
  				val a = ip(10)
  				a + 10
  			}""") should yieldCompileError("impure method call inside the pure method 'a'")
  }

  @Test def pureFunctionMustNotCallImpureFunctionMiddleWithAssignment2 {
    code("""
  			@pure def a = {
  				val a: Int = ip(10)
  				a + 10
  			}""") should yieldCompileError("impure method call inside the pure method 'a'")
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
  	  """) should yieldCompileError("access to non-local var inside the pure method 'A.a'")
  }

  @Test def pureFunctionMustNotAccessVarsPrivateThis {
    code("""
  	     class A {
  	       private[this] var x: Int = 0
  			   @pure def a = x
         }
  	  """) should yieldCompileError("access to non-local var inside the pure method 'A.a'")
  }

  @Test def pureFunctionMustNotModifyVars {
    code("""
  			 class A {
  			  var x: Int = 0
  			  @pure def a(i: Int) { x = i }
  			 }
  	  """) should yieldCompileError("write to non-local var inside the pure method 'A.a'")
  }

  @Test def pureFunctionMustNotModifyVarsPrivateThis {
    code("""
  			 class A {
  			  private[this] var x: Int = 0
  			  @pure def a(i: Int) { x = i }
  			 }
  	  """) should yieldCompileError("write to non-local var inside the pure method 'A.a'")
  }

  @Test def pureFunctionMustNotAccessVarInOtherClasses {
    code("""
        object A { var a = 0 }
        class B {
        	@pure def x = A.a + 10
        }
      """) should yieldCompileError("access to non-local var inside the pure method 'B.x'")
  }

  @Test def pureFunctionMustNotWriteVarInOtherClasses {
    code("""
        object A { var a = 0 }
        class B {
        	@pure def x = {
        		A.a = 1
        		2
    			}
        }
      """) should yieldCompileError("write to non-local var inside the pure method 'B.x'")
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
      """) should yieldCompileError("access to non-local var inside the pure method 'b'")
  }

  @Test def pureFunctionMayNotWriteVarsOfOuterFunction {
    code("""
  			@pure def a = {
  			  var x = 10
  		  	@pure def b { x = 1 }
  		  	b
  	    	x
  			}
  	  """) should yieldCompileError("write to non-local var inside the pure method 'b'")
  }

  @Test def pureFunctionMayAccessValsOfOuterFunction {
    code("""
        @pure def a = {
          val x = 10
          @pure def b = x + 1
        	b
    	  }
      """) should compile
  }
}