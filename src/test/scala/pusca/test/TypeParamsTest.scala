package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class TypeParamsTest extends JUnitSuite with ShouldMatchersForJUnit {
  private def code(c: String) = {
    val p = """
      def assertPure(purity: Purity) = if (purity != AlwaysPure) throw new AssertionError("expected AlwaysPure but was "+purity) 
      def assertImpure(purity: Purity) = if (purity != AlwaysImpure) throw new AssertionError("expected AlwaysImpure but was "+purity) 
      """
    PluginTester.code(p + "\n" + c)
  }

  @Test def sideEffectFreeExtendOfNormal {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        class LenFun extends Fun[String, Int @sideEffectFree] {
          override def apply(s: String) = s.length
        }
        val lf = new LenFun
        assertPure(purityOf(lf.apply("Test")))
        """) should compile
  }
  @Test def sideEffectFreeExtendViaType {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        class LenFun extends PureFun[String, Int] {
          override def apply(s: String) = s.length
        }
        val lf = new LenFun
        assertPure(purityOf(lf.apply("Test")))
        """) should compile
  }
  @Test def sideEffectFreeExtendOfNormalWithSugaredApply {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        class LenFun extends Fun[String, Int @sideEffectFree] {
          override def apply(s: String) = s.length
        }
        val lf = new LenFun
        assertPure(purityOf(lf("Test")))
        """) should compile
  }
  @Test def sideEffectFreeExtendOfNormalObject {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        object LenFun extends Fun[String, Int @sideEffectFree] {
          override def apply(s: String) = s.length
        }
        assertPure(purityOf(LenFun("Test")))
        """) should compile
  }

  @Test def sideEffectFreeExtendOfNormalCastedToGenericShouldBeImpure {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        object LenFun extends Fun[String, Int @sideEffectFree] {
          override def apply(s: String) = s.length
        }
        val f: Fun[String, Int] = LenFun
        assertImpure(purityOf(f("Test")))
        """) should compile
  }

  @Test def sideEffectFreeExtendViaTypeCastedToFunShouldBeImpure {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        class LenFun extends PureFun[String, Int] {
          override def apply(s: String) = s.length
        }
        val lf: Fun[String,Int] = new LenFun
        assertImpure(purityOf(lf.apply("Test")))
        """) should compile
  }
  @Test def sideEffectFreeExtendViaTypeCastedToPureFunShouldBePure {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        class LenFun extends PureFun[String, Int] {
          override def apply(s: String) = s.length
        }
        val lf: PureFun[String,Int] = new LenFun
        assertPure(purityOf(lf.apply("Test")))
        """) should compile
  }

}