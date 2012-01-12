package pusca.test.old

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
  @Test def anonFunctionWithoutSideEffectIsPure7 {
    code("""
        @pure def len(s: String): Int = 10
        val f: String -> Int = s => len(s)
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
  @Test def anonFunctionWithSideEffectIsImpureWithAnnotation {
    code("""
        @impure def ip(s: String): Int = s.length
        val f: String => Int @sideEffect = s => ip(s)
    """) should compile
  }
  @Test def anonFunctionWithSideEffectIsImpure {
    code("""
        @impure def ip(s: String): Int = s.length
        val f: String ==> Int = s => ip(s)
    """) should compile
  }
  @Test def anonFunctionWithSideEffectIsImpureCheckImpure {
    code("""
        @impure def ip(s: String): Int = s.length
        val f: String ==> Int = s => ip(s)
        @pure def p: Int = f("Ho")
        """) should yieldCompileError("impure method call to apply inside the pure method p")
  }
  @Test def anonFunctionWithSideEffectIsImpure2 {
    code("""
        @impure def ip(s: String) = s.length
        val f: String ==> Int = s => ip(s)
        """) should compile
  }
  @Test def anonFunctionWithSideEffectIsImpure2CheckImpure {
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

  @Test def assignPureDefToPureFunVal {
    code("""
        @pure def a(v: String): Int = v.length
        val f: String -> Int = a _
        """) should compile
  }
  @Test def assignPureDefToFunVal {
    code("""
        @pure def a(v: String): Int = v.length
        val f: String => Int = a _
        """) should compile
  }
  @Test def assignImpureDefToImpureFunVal {
    code("""
        @impure def ia(v: String): Int = v.length
        val f: String => Impure[Int] = ia _
        """) should compile
  }
  @Test def assignImpureDefToImpureFunVal2 {
    code("""
        @impure def ia(v: String): Int = v.length
        val f: String ==> Int = ia _
        """) should compile
  }
  @Test def assignImpureDefToFunVal {
    code("""
        @impure def ia(v: String): Int = v.length
        val f: String => Int = ia _
        """) should compile
  }
  @Test def assignImpureDefToPureFunVal {
    code("""
        @impure def ia(v: String): Int = v.length
        val f: String -> Int = ia _
        """) should yieldCompileError("type mismatch")
  }

  @Test def applyImpureInInfered {
    code("""
        def meth[A](f: String => A): A = f("Hi")
        """) should compile
  }

  @Test def applyPureInPure {
    code("""
        @pure def meth[B](f: String => Pure[B]): B = f("Hi")
        """) should compile
  }
  @Test def applyPureInPure2 {
    code("""
        @pure def meth[B](f: String -> B): B = f("Hi")
        """) should compile
  }
  @Test def applyPureInPure3 {
    code("""
        @pure def meth[B](f: String => B @sideEffectFree): B = f("Hi")
        """) should compile
  }

  @Test def applyPureInInferedThatShouldBePure {
    code("""
        def meth[B](f: String -> B): B = f("Hi")
        @pure def p = meth(_.length)
        """) should compile
  }

}