package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._
import pusca._
import scala.annotation.StaticAnnotation

class ParametrizedTypesTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def methodWithOneParameterPure {
    code("""
        @impureIf('A) def exec[A](f: () => A): String = {
        	f()
        	"Hi"
    		}
        @pure def p = exec[Int] { () => 10 } 
        """) should compile
  }
  @Test def methodWithOneParameterImpure {
    code("""
        @impureIf('A) def exec[A](f: () => A): String = {
        	f()
        	"Hi"
    		}
        @pure def p = exec[Int @sideEffect] { () => 10 } 
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  @Test def methodWithOneParameterNoDeclaration {
    code("""
        def exec[A](f: () => A): String = {
        	f()
        	"Hi"
    		}
        """) should yieldCompileError("impure method call inside the pure method 'exec'")
  }

  @Test def methodWithOneParameterRvPure {
    code("""
    		@impureIf('A) def x[A](f: String => A) = f("Hi")
        @pure def p = {
        	val f: String => Int = s => s.length
        	x[Int](f)
    		}
        """) should compile
  }
  
  @Test def methodWithOneParameterRvImpure {
    code("""
    		@impureIf('A) def x[A](f: String => A) = f("Hi")
        @pure def p = {
        	val f: String => Int @sideEffect = s => s.length
        	x[Int @sideEffect](f)
    		}
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  
  @Test def boxForFunctionPure {
    code("""
        case class FBox[A,B](f: A => B) {
        	@impureIf('B) def apply(a: A): B = f(a)
    		}
       	val box = FBox[String,Int](_.length)
        @pure def p = box("Hi")
        """) should compile
  }
  @Test def boxForFunctionImpure {
    code("""
        case class FBox[A,B](f: A => B) {
        	@impureIf('B) def apply(a: A): B = f(a)
    		}
       	val box = FBox[String,Int @sideEffect](_.length)
        @pure def p = box("Hi")
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  
  @Test def boxWithPreFunPure {
    code("""
        case class PFB[A,B,C](f: A => B, pf: () => C) {
        	@impureIf('B,'C) def apply(a: A) = {
        		pf()
        		f(a)
    			}
    		}
        val box = PFB[String,Int,Unit](_.length, () => ())
        @pure def p = box("Hi")
        """) should compile
  }
  @Test def boxWithPreFunImpureFun {
    code("""
        case class PFB[A,B,C](f: A => B, pf: () => C) {
        	@impureIf('B,'C) def apply(a: A) = {
        		pf()
        		f(a)
    			}
    		}
        val box = PFB[String,Int @sideEffect,Unit](_.length, () => ())
        @pure def p = box("Hi")
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  @Test def boxWithPreFunImpurePreFun {
    code("""
        case class PFB[A,B,C](f: A => B, pf: () => C) {
        	@impureIf('B,'C) def apply(a: A) = {
        		pf()
        		f(a)
    			}
    		}
        val box = PFB[String,Int,Unit @sideEffect](_.length, () => ())
        @pure def p = box("Hi")
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  @Test def boxWithPreFunUndeclaredDependencyOnPurenessOfB {
    code("""
        case class PFB[A,B,C](f: A => B, pf: () => C) {
        	@impureIf('B) def apply(a: A) = {
        		pf()
        		f(a)
    			}
    		}
        """) should yieldCompileError("impure method call inside the pure method 'apply'")
  }
}