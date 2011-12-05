package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import PluginTester._

class TailcallsTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def pureMethodDoesNotPreventTailcallOptimization {
    code("""
        @pure @annotation.tailrec def printAll(l: List[String]): Unit = l match {
        	case e :: r => printAll(r)
        	case Nil => ()
    		}
        """) should compile
  }

  @Test def impureMethodDoesNotPreventTailcallOptimization {
    code("""
        @impure def print(e: String) = ()
        @impure @annotation.tailrec def printAll(l: List[String]): Unit = l match {
        	case e :: r =>
        		print(e)
        		printAll(r)
        	case Nil => ()
    		}
        """) should compile
  }

  @Test def impureIfMethodDoesNotPreventTailcallOptimization {
    code("""
        trait X[A] {
	        @impureIf('A) def print(e: String) = ()
	        @impureIf('A) @annotation.tailrec private def printAll(l: List[String]): Unit = l match {
	        	case e :: r =>
	        		print(e)
	        		printAll(r)
	        	case Nil => ()
    			}
    		}
        """) should compile
  }

  @Test def declarePureMethodDoesNotPreventTailcallOptimization {
    code("""
        @impure def print(e: String) = ()
        @declarePure @annotation.tailrec def printAll(l: List[String]): Unit = l match {
        	case e :: r =>
        		print(e)
        		printAll(r)
        	case Nil => ()
    		}
        """) should compile
  }
}
