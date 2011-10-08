package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class OverrideTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def pureMayOverridePureAbstract {
    code("""
    		trait A { @pure def a: String }
    		class B extends A { override def a = "Hi" }""") should compile
  }
  
  @Test def pureMayOverridePureAbstract2 {
    code("""
    		trait A { def a: String }
    		class B extends A { override def a = "Hi" }""") should compile
  }
  
  @Test def pureMayOverridePure {
    code("""
  			trait A { def a: String }
  			class B extends A { override def a = "Hi" }
  			class C extends B { override def a = super.a + "!" }""") should compile
  }

  @Test def impureMayOverrideImpure {
    code("""
    		trait A { @impure def a: String }
    		class B extends A { @impure override def a = "Hi" }""") should compile
  }

  @Test def pureMayOverrideImpure {
    code("""
    		trait A { @impure def a: String }
    		class B extends A { override def a = "Hi" }""") should compile
  }

  @Test def pureMayOverrideImpureNonAbstract {
    code("""
  			trait A { @impure def a: String }
  			class B extends A { @impure override def a = "Hi" }
  			class C extends B { override def a = "Ho" }""") should compile
  }

  @Test def pureMayOverrideImpureButNotCallSuper {
    code("""
    		trait A { @impure def a: String }
    		class B extends A { @impure override def a = "Hi" }
    		class C extends B { override def a = super.a + "!" }""") should
    			yieldCompileError("impure function call inside the pure function 'a'")
  }

  @Test def impureMayNotOverridePure {
    code("""
  			trait A { def a: String }
  			class B extends A { @impure override def a = "Hi" }""") should
  				yieldCompileError("cannot override pure function 'a'")
  }

  @Test def impureMayNotOverridePureNonAbstract {
    code("""
  			trait A { def a: String = "Ho" }
  	    class B extends A { @impure override def a = "Hi" }""") should
  	    	yieldCompileError("cannot override pure function 'a'")
  }
}