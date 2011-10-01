package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test

class OverrideTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def pureMayOverridePureAbstract {
    PluginTester("""
    		trait A { def a: String }
    		class B extends A { override def a = "Hi" }""") should be('compiled)
  }

  @Test def pureMayOverridePure {
    PluginTester("""
  			trait A { def a: String }
  			class B extends A { override def a = "Hi" }
  			class C extends B { override def a = super.a + "!" }""") should be('compiled)
  }

  @Test def impureMayOverrideImpure {
    PluginTester("""
    		trait A { @impure def a: String }
    		class B extends A { @impure override def a = "Hi" }""") should be('compiled)
  }

  @Test def pureMayOverrideImpure {
    PluginTester("""
    		trait A { @impure def a: String }
    		class B extends A { override def a = "Hi" }""") should be('compiled)
  }

  @Test def pureMayOverrideImpureNonAbstract {
    PluginTester("""
  			trait A { @impure def a: String }
  			class B extends A { @impure override def a = "Hi" }
  			class C extends B { override def a = "Ho" }""") should be('compiled)
  }

  @Test def pureMayOverrideImpureButNotCallSuper {
    val e = PluginTester("""
    		trait A { @impure def a: String }
    		class B extends A { @impure override def a = "Hi" }
    		class C extends B { override def a = super.a + "!" }""").compileErrors
    e should have size (1)
    e.head should include("Impure function call")
  }

  @Test def impureMayNotOverridePure {
    val e = PluginTester("""
  			trait A { def a: String }
  			class B extends A { @impure override def a = "Hi" }""").compileErrors
    e should have size (1)
    e.head should include("cannot override pure")
  }
  
  @Test def impureMayNotOverridePureNonAbstract {
    val e = PluginTester("""
  			trait A { def a: String = "Ho" }
  	    class B extends A { @impure override def a = "Hi" }""").compileErrors
    e should have size (1)
    e.head should include("cannot override pure")
  }
}