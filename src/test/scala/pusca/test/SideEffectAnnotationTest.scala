package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class SideEffectAnnotationTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def canReturnIntWhenDefinedIntSideEffect {
    code("def a: Int @sideEffect = 10") should compile
  }

  @Test def cannotReturnIntSideEffectWhenDefinedIntAndPure {
    code("""
        def a: Int @sideEffect = 10
        @pure def b: Int = a""") should yieldCompileError("impure method call inside the pure method 'b'")
  }

  @Test def cannotReturnIntSideEffectWhenDefinedIntAndReturningInt {
    code("""
        def a: Int @sideEffect = 10
        def b: Int = {
        	a
        	10
    		}""") should yieldCompileError("impure method call inside the pure method 'b'")
  }

  @Test def canReturnIntSideEffectWhenOnlyDefinedInt1 {
    code("""
        def b: Int = addSideEffect(10)""") should yieldCompileError("impure method call inside the pure method 'b'")
  }
  @Test def cannotReturnIntSideEffectWhenDefinedInt2 {
    code("""
  			def a: Int @sideEffect = 10
  			def b: Int = a""") should yieldCompileError("impure method call inside the pure method 'b'")
  }

  @Test def methodNotSideEffectOverrideSideEffect {
    code("""
        trait A { def a: Int @sideEffect }
        trait B extends A { override def a: Int = 10 }""") should compile
  }

  @Test def methodWithSideEffectCannotOverrideMethodWithout {
    code("""
        trait A { def a: Int }
        trait B extends A { override def a: Int @sideEffect }""") should yieldCompileError("overriding method a in trait A")
  }

  @Test def sideEffectIsPreserved {
    code("""
        def a: Int @sideEffect = 10
        def b = a""") should compile
    code("""
        def a: Int @sideEffect = 10
        def b = a
        def c: Int = b""") should yieldCompileError("impure method call inside the pure method 'c'")
  }

  @Test def sideEffectIsAValidTypeParameter {
    code("new Function0[Unit @sideEffect] { override def apply() = () }") should compile
  }
  @Test def sideEffectIsAValidTypeParameter2 {
    code("new Function0[String @sideEffect] { override def apply() = \"Ho\" }") should compile
  }

  @Test def sideEffectIsAValidTypeParameterForMethod {
    code("""
        def a[A](a: A) = 10
        def b = a[String @sideEffect]("hi")
        """) should compile
  }

  @Test def sideEffectIsAValidFunctionTypeParamter {
    code("new Function1[Int,String @sideEffect] { override def apply(i: Int) = \"hi\" }") should compile
  }

  @Test def pureReturnTypeConformsToSideEffectTypeParameter {
    code("new Function1[Int, String @sideEffect] { override def apply(i: Int): String = \"hi\" }") should compile
  }

  @Test def sideEffectReturnTypeDoesNotConformToPureTypeParameter {
    code("new Function1[Int, String] { override def apply(i: Int): String @sideEffect = \"hi\" }") should
      yieldCompileError("overriding method apply in trait Function1")
  }

  @Test def sideEffectInTypeParameterIsPreserved {
    //not sure why exactly we need to call a as a{String @sideEffect]. Must be something with the type inference.
    code("""
          def a[A](f: Int => A): A = f(10)
          def b = a[String @sideEffect](new Function1[Int,String @sideEffect] {
          		override def apply(i: Int) = i.toString
      			})
          """) should compile
    code("""
          def a[A](f: Int => A): A = f(10)
          def b = a[String @sideEffect](new Function1[Int,String @sideEffect] {
          		override def apply(i: Int) = i.toString
      			})
          def c: String = b 
          """) should yieldCompileError("impure method call inside the pure method 'c'")
  }

  @Test def methodWithIfWhereBothBranchsAreSideEffectIsImpure {
    code("""
        @impure def ip = "Hello"
        def m(v: Boolean) = {
    		if (v) ip
    		else ip
    	}
        @pure def p = m(true)
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  @Test def methodWithIfWhereThenBranchIsSideEffectIsImpure {
    code("""
        @impure def ip = "Hello"
        def m(v: Boolean) = {
    		if (v) ip
    		else "Hi"
    	}
        @pure def p = m(true)
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  @Test def methodWithIfWhereElseBranchIsSideEffectIsImpure {
    code("""
        @impure def ip = "Hello"
        def m(v: Boolean) = {
    		if (v) "Hi"
    		else ip
    	}
        @pure def p = m(true)
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  @Test def methodWithIfWhereNoBranchsIsSideEffectIsPure {
    code("""
        def m(v: Boolean) = {
    		if (v) "Hi"
    		else "Ho"
    	}
        @pure def p = m(true)
        """) should compile
  }

  @Test def methodWithMatchWhereAllCasesHaveSideEffectIsImpure {
    code("""
        @impure def ip = "Hello"
        def m(i: Int) = i match {
    		case 1 => ip 
    		case 2 => ip 
    		case _ => ip 
    	}
        @pure def p = m(2)
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  @Test def methodWithMatchWhereOneCaseHasSideEffectIsImpure {
    code("""
        @impure def ip = "Hello"
        def m(i: Int) = i match {
    		case 1 => ip 
    		case 2 => "Hi" 
    		case _ => "Ho"
    	}
        @pure def p = m(2)
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  @Test def methodWithMatchWhereOneCaseHasSideEffectIsImpure2 {
    code("""
        @impure def ip = "Hello"
        def m(i: Int) = i match {
    		case 1 => "Hi" 
    		case 2 => "Hi" 
    		case _ => ip
    	}
        @pure def p = m(2)
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  @Test def methodWithMatchWhereNoCaseHasSideEffectIsPure {
    code("""
        def m(i: Int) = i match {
    		case 1 => "Hu" 
    		case 2 => "Hi" 
    		case _ => "Ho"
    	}
        @pure def p = m(2)
        """) should compile
  }

  @Test def methodWithOptionGetOrElseHasSideEffectIfGetOrElseHas {
    code("""
        @impure def ip = "Hello"
        def m(o: Option[String]) = {
    		o.getOrElse[String @sideEffect](ip)
    	}
        @pure def p = m(Some("Hi"))
        """) should yieldCompileError("impure method call inside the pure method 'p'")
  }
  @Test def methodWithOptionGetOrElseHasNoSideEffectIfGetOrElseHasNot {
    code("""
        def ip = "Hello"
        def m(o: Option[String]) = {
    		o.getOrElse(ip)
    	}
        @pure def p = m(Some("Hi"))
        """) should compile
  }
}