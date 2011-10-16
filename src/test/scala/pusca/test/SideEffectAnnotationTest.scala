package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class SideEffectAnnotationTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def canReturnIntWhenDefinedIntSideEffect {
    code("def a: Int @sideEffect = 10") should compile
  }

  @Test def cannotReturnIntSideEffectWhenDefinedInt {
    code("""
        def a: Int @sideEffect = 10
        def b: Int = a""") should yieldCompileError("type mismatch")
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
        def c: Int = b""") should yieldCompileError("type mismatch")
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
          """) should yieldCompileError("type mismatch")
  }
}