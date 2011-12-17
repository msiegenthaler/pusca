package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class FunctionTypeTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def assignPlainToPlain {
    code("""
        trait X {
        	def e: String => Int
        	def f: String => Int = e 
    		}""") should compile
  }
  @Test def assignSideEffectToPlain {
    code("""
        trait X {
    			def e: String => Impure[Int]
        	def f: String => Int = e
    		}""") should compile
  }
  @Test def assignSideEffectFreeToPlain {
    code("""
        trait X {
    			def e: String => Pure[Int]
        	def f: String => Int = e
    		}""") should compile
  }

  @Test def assignPlainToSideEffect {
    code("""
        trait X {
    			def e: String => Int
        	def f: String => Impure[Int] = e
    		}""") should compile
  }
  @Test def assignSideEffectToSideEffect {
    code("""
        trait X {
    			def e: String => Impure[Int] 
    			def f: String => Impure[Int] = e
    		}""") should compile
  }
  @Test def assignSideEffectFreeToSideEffect {
    code("""
        trait X {
    			def e: String => Pure[Int] 
    			def f: String => Impure[Int] = e
    		}""") should compile
  }
  @Test def assignPlainToSideEffect2 {
    code("""
        trait X {
    			def e: String => Int
        	def f: String ==> Int = e
    		}""") should compile
  }
  @Test def assignSideEffectToSideEffect2 {
    code("""
        trait X {
    			def e: String => Impure[Int] 
    			def f: String ==> Int = e
    		}""") should compile
  }
  @Test def assignSideEffectFreeToSideEffect2 {
    code("""
        trait X {
    			def e: String => Pure[Int] 
    			def f: String ==> Int = e
    		}""") should compile
  }

  @Test def assignPlainToSideEffectFree {
    code("""
        trait X {
    			def e: String => Int
    			def f: String => Pure[Int] = e
    		}""") should yieldCompileError("type mismatch")
  }
  @Test def assignSideEffectToSideEffectFree {
    code("""
        trait X {
    			def e: String => Impure[Int]
    			def f: String => Pure[Int] = e
    		}""") should yieldCompileError("type mismatch")
  }
  @Test def assignSideEffectFreeToSideEffectFree {
    code("""
        trait X {
    			def e: String => Pure[Int]
    			def f: String => Pure[Int] = e
    		}""") should compile
  }
  @Test def assignPlainToSideEffectFree2 {
    code("""
        trait X {
    			def e: String => Int
    			def f: String -> Int = e
    		}""") should yieldCompileError("type mismatch")
  }
  @Test def assignSideEffectToSideEffectFree2 {
    code("""
        trait X {
    			def e: String => Impure[Int]
    			def f: String -> Int = e
    		}""") should yieldCompileError("type mismatch")
  }
  @Test def assignSideEffectFreeToSideEffectFree2 {
    code("""
        trait X {
    			def e: String => Pure[Int]
    			def f: String -> Int = e
    		}""") should compile
  }

  @Test def anonFunctionWithoutSideEffectIsPure {
    code("""
        @pure def len(s: String): Int = s.length
        val f: String -> Int = s => len(s)
        """) should compile
  }
  @Test def anonFunctionWithoutSideEffectIsPure2 {
    code("""
        @pure def len(s: String): Int = s.length
        val f: String => Int @sideEffectFree = s => len(s)
        """) should compile
  }
  @Test def anonFunctionWithoutSideEffectIsPure3 {
    code("""
        val f: String => Int @sideEffectFree = s => s.length
        """) should compile
  }
  @Test def anonFunctionWithoutSideEffectIsPure4 {
    code("""
        val f: String => Pure[Int] = s => s.length
        """) should compile
  }
  
  @Test def anonFunctionWithoutSideEffectCanBeAssignedToPlain {
    code("""
        val f: String => Int = s => s.length
        """) should compile
  }
  @Test def anonFunctionWithoutSideEffectCanBeAssignedToImpure {
    code("""
        val f: String ==> Int = s => s.length
        """) should compile
  }
  
  @Test def anonFunctionWithSideEffectIsImpure {
    code("""
        @impure def ip(s: String) = s.length
        val f: String ==> Int = s => ip(s)
        @pure def p = f("Ho")
        """) should yieldCompileError("impure method call to apply inside the pure method p")
  }
  @Test def anonFunctionWithSideEffectCanBeAssignedToImpure {
    code("""
        @impure def ip(s: String) = s.length
        val f: String ==> Int = s => ip(s)
        """) should compile
  }
  @Test def anonFunctionWithSideEffectCanBeAssignedToPlain {
    code("""
        @impure def ip(s: String) = s.length
        val f: String => Int = s => ip(s)
        """) should compile
  }
  @Test def anonFunctionWithSideEffectCannotBeAssignedToPure {
    code("""
        @impure def ip(s: String) = s.length
        val f: String -> Int = s => ip(s)
        """) should yieldCompileError("type mismatch")
  }
}