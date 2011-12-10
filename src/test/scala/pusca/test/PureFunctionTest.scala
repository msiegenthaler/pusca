package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class PureFunctionTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def unitFunIsPure {
    code("@pure def uf[A](a: A): A = a") should compile
  }
  @Test def executePureFunIsPure {
    code("@pure def uf[A,B](a: A)(f: A -> B): B = f(a)") should compile
  }

  @Test def boxMapToUnit {
    code("""
        case class X[A](value: A) {
        	@pure def map[B](f: A -> B): X[B] = X(f(value))
    		}
        @pure def test {
        	val x = X("Hello")
        	val y = x.map(v => v)
        	val z: X[String] = y
    		}
        """) should compile
  }
  @Test def boxMapToOther {
    code("""
        case class X[A](value: A) {
        	@pure def map[B](f: A -> B): X[B] = X(f(value))
    		}
        @pure def test {
        	val x = X("Hello")
        	val y = x.map(_.length)
    		}
        """) should compile
  }

  //simplified version of scalaz making use of PureFunction (A -> B)
  private val functorDefinition = """
        trait Functor[F[_]] {
        	def fmap[A,B](r: F[A], f: A -> B): F[B]
    		}
        object Functor {
        	implicit def listFunctor: Functor[List] = new Functor[List] {
        		override def fmap[A,B](r: List[A], f: A -> B): List[B] = r map f 
    			}
    		}
        trait PimpedType[X] { val value: X }
        sealed trait MA[M[_],A] extends PimpedType[M[A]] {
        	def o[B](f: A -> B)(implicit t: Functor[M]) = t.fmap(value, f)
    		}
			  implicit def maImplicit[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
			    val value = a
			  }
        
    		import Functor._
    """

  @Test def functorWithAnon {
    code(functorDefinition + """
        @pure def x {
        	val l = List(1, 2, 3) o (_ * 2)
        	val m: List[Int] = l
    		}
        """) should compile
  }
  
  @Test def functorWithFun {
    code(functorDefinition + """
        @pure def double(a: Int) = a * 2
        @pure def x {
        	val l = List(1, 2, 3) o double
        	val m: List[Int] = l
    		}
        """) should compile
  }
  
  @Test def functorCannotHaveImpureFMap {
    code(functorDefinition + """
        @impure def ip[A](in: A) = println("Hi "+in)
        @pure def x {
        	val l = List(1, 2, 3) o ip
        	val m: List[Int] = l
    		}
        """) should yieldCompileError("polymorphic expression cannot be instantiated to expected type")
  }
}