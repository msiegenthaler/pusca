package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test

class ConstructorTests extends JUnitSuite with ShouldMatchersForJUnit {

  @Test def pureFunctionMayCallPureConstructor {
    PluginTester("""
    		class A { val x = "Hi" }
    		def makeA = new A""") should be('compiled)
  }
  @Test def pureFunctionMayCallPureConstructor2 {
    PluginTester("""
  			class A(a: String)
  			def makeA = new A("Hi")""") should be('compiled)
  }

  @Test def impureFunctionMayCallImpureConstructor {
    PluginTester("""
  			class A(var x: String)
  			@impure def makeA = new A("hi")""") should be('compiled)
  }

  @Test def impureFunctionMayCallPureConstructor {
    PluginTester("""
  			class A { val x = "Hi" }
  			@impure def makeA = new A""") should be('compiled)
  }

  @Test def constuctorThatAccessesVarIsImpure {
    val e = PluginTester("""
    		class A { var x = "Hi" }
    		def makeA = new A""").compileErrors
    e should have size (1)
    e.head should include("Impure function call to A.<init>")
  }

  @Test def constuctorThatAccessesVarIsImpure2 {
    val e = PluginTester("""
  			class A(var x: String)
  			def makeA = new A("hi")""").compileErrors
    e should have size (1)
    e.head should include("Impure function call to A.<init>")
  }

  @Test def constructorThatCallsImpureMethodIsImpure {
    val e = PluginTester("""
  			class A { println("hi") }
  			def makeA = new A""").compileErrors
    e should have size (1)
    e.head should include("Impure function call to A.<init>")
  }

  @Test def impureConstructorsGetInherited = {
    val e = PluginTester("""
  			class A { println("hi") }
  			class B extends A
  			def makeB = new B""").compileErrors
    e should have size (1)
    e.head should include("Impure function call to B.<init>")
  }

  @Test def impureConstructorsGetInherited2 = {
    val e = PluginTester("""
  			class A { var a = 0 }
  			class B extends A
  			def makeB = new B""").compileErrors
    e should have size (1)
    e.head should include("Impure function call to B.<init>")
  }

  @Test def impureConstructorsGetInheritedByAnonClasses = {
    val e = PluginTester("""
				trait A { val a: String; println("Hi") }
 				def makeA = new A { override val a = "Hi" }""").compileErrors
    e should have size (1)
    e.head should include("Impure function call to $anon.<init>")
  }

  @Test def pureConstructorsGetInheritedByAnonClasses = {
    PluginTester("""
 				trait A { val a: String }
  			def makeA = new A { override val a = "Hi" }""") should be('compiled)
  }

}