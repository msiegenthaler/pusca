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

  @Test def pureMayOverrideImpureButNotCallSuperMiddle {
    code("""
    		trait A { @impure def a: String }
    		class B extends A { @impure override def a = "Hi" }
    		class C extends B {
        	@pure override def a = {
        		val v = super.a 
        		v + "!"
    			}
        }""") should yieldCompileError("impure method call inside the pure method 'C.a'")
  }
  
  @Test def pureMayOverrideImpureButNotCallSuperEnd {
    code("""
    		trait A { @impure def a: String }
    		class B extends A { @impure override def a() = "Hi" }
    		class C extends B {
        	@pure override def a() = super.a() + "!" 
        }""") should yieldCompileError("impure method call inside the pure method 'C.a'")
  }

  @Test def pureMayOverrideImpureButNotCallSuperEnd2 {
    code("""
    		trait A { @impure def a: String }
    		class B extends A { @impure override def a = "Hi" }
    		class C extends B {
        	@pure override def a = super.a + "!" 
        }""") should yieldCompileError("impure method call inside the pure method 'C.a'")
  }

  @Test def impureMayNotOverridePureSideEFfect {
    code("""
  			trait A { def a: String }
  			class B extends A { override def a: String @sideEffect = "Hi" }""") should
  				yieldCompileError("overriding method a in trait A of type => String")
  }
  
  @Test def impureMayNotOverridePureImpure {
    code("""
  			trait A { def a: String }
  			class B extends A { @impure override def a = "Hi" }""") should
  				yieldCompileError("type mismatch")
  }
  
  @Test def impureMayNotOverridePureNonAbstractSideEffect {
    code("""
  			trait A { def a: String = "Ho" }
  	    class B extends A { override def a: String @sideEffect = "Hi" }""") should
  	    	yieldCompileError("overriding method a in trait A of type => String")
  }

  @Test def impureMayNotOverridePureNonAbstractImpure {
    code("""
  			trait A { def a: String = "Ho" }
  	    class B extends A { @impure override def a = "Hi" }""") should
  	    	yieldCompileError("type mismatch")
  }
}