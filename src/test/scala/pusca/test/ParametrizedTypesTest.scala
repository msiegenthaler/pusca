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
        """) should yieldCompileError("impure method call to exec inside the pure method p")
  }

  @Test def methodWithOneParameterNoDeclaration {
    code("""
        def exec[A](f: () => A): String = {
        	f()
        	"Hi"
    		}
        """) should yieldCompileError("impure method call to apply inside the pure method exec")
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
        """) should yieldCompileError("impure method call to x inside the pure method p")
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
        """) should yieldCompileError("impure method call to apply inside the pure method p")
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
        """) should yieldCompileError("impure method call to apply inside the pure method p")
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
        """) should yieldCompileError("impure method call to apply inside the pure method p")
  }
  @Test def boxWithPreFunUndeclaredDependencyOnPurenessOfB {
    code("""
        case class PFB[A,B,C](f: A => B, pf: () => C) {
        	@impureIf('B) def apply(a: A) = {
        		pf()
        		f(a)
    			}
    		}
        """) should yieldCompileError("impure method call to apply inside the pure method PFB.apply")
  }

  @Test def renamedTypeParametersWithSuperTrait {
    code("""
        trait A[Z] {
        	val f: () => Z
        	@impureIf('Z) def exec: Unit = f() 
    		}
        class B[Y](override val f: () => Y) extends A[Y]
        @pure def p = {
        	val b = new B(() => "Hi")
        	b.exec
    		}
        """) should compile
  }
  @Test def renamedTypeParametersWithSuperTraitImpure {
    code("""
        trait A[Z] {
        	val f: () => Z
        	@impureIf('Z) def exec: Unit = f() 
    		}
        class B[Y](override val f: () => Y) extends A[Y]
        @pure def p = {
        	val b = new B[String @sideEffect](() => "Hi")
        	b.exec
    		}
        """) should yieldCompileError("impure method call to exec inside the pure method p")
  }

  @Test def caseClassWithTypeParameter {
    code("case class Res[Z](value: Z)") should compile
  }

  @Test def renameTypeParametersWithType {
    code("""
        trait Res[Z]
        class ResImpl[A](a: A) extends Res[A]
        type Fun[X] = X => Res[X]
        trait Y[B] {
    			def handle(f: Res[B]) = ()
    		}
        def run[E](a: E) = a 
        trait X[C] extends Y[C] {
    			val f: Fun[C] = x => new ResImpl(x)
	    		def exec(c: C) = run {
        		handle(f(c))
    			}
    		}
        """) should compile
  }

  @Test def outerClassTypeParameter {
    code("""
        trait A[Z] {
        	trait Fun[A] extends Function1[A,Z]
        	@impureIf('Z) def exec(f: Fun[String]) = f("Hi")
    		}""") should compile
  }
  @Test def outerOuterClassTypeParameter {
    code("""
        trait A[Z] {
        	trait Fun[A] extends Function1[A,Z]
        	trait B[Y] {
        		val a: Y
    				@impureIf('Z) def exec(f: Fun[Y]) = f(a)
    			}
    		}""") should compile
  }

  @Test def outerMethodTypeParameter {
    code("""
        def o[Z] {
        	trait Fun[A] extends Function1[A,Z]
        	@impureIf('Z) def exec(f: Fun[String]) = f("Hi")
        	()
    		}""") should compile
  }
  @Test def outerOuterMethodTypeParameter {
    code("""
        def o[Z] {
        	trait Fun[A] extends Function1[A,Z]
        	def o2[Y](a: Y) {
        		@impureIf('Z) def exec(f: Fun[Y]) = f(a)
        		()
    			}
        	()
    		}""") should compile
  }
}