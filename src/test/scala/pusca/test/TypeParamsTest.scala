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
        """) should compileAndRun
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
        """) should compileAndRun
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
        """) should compileAndRun
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
        """) should compileAndRun
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
        """) should compileAndRun
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
        """) should compileAndRun
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
        """) should compileAndRun
  }

  @Test def sideEffectFreeExtendViaTwoType {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        type LenFun = PureFun[String,Int]
        object LenFunImpl extends LenFun {
          override def apply(s: String) = s.length
        }
        val lf = LenFunImpl
        assertPure(purityOf(lf("Test")))
        """) should compileAndRun
  }
  @Test def sideEffectFreeExtendViaTwoTypeAsLenFun {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        type LenFun = PureFun[String,Int]
        object LenFunImpl extends LenFun {
          override def apply(s: String) = s.length
        }
        val lf: LenFun = LenFunImpl
        assertPure(purityOf(lf("Test")))
        """) should compileAndRun
  }

  @Test def sideEffectFreeExtendViaTwoTypeAsFunShouldBeImpure {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        type LenFun = PureFun[String,Int]
        object LenFunImpl extends LenFun {
          override def apply(s: String) = s.length
        }
        val lf: Fun[String,Int] = LenFunImpl
        assertImpure(purityOf(lf("Test")))
        """) should compileAndRun
  }
  @Test def sideEffectFreeExtendViaTwoTypeAsFunSubtypeShouldBeImpure {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type GenLenFun = Fun[String,Int]
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        type LenFun = PureFun[String,Int]
        object LenFunImpl extends LenFun {
          override def apply(s: String) = s.length
        }
        val lf: GenLenFun = LenFunImpl
        assertImpure(purityOf(lf("Test")))
        """) should compileAndRun
  }

  @Test def sideEffectFreeExtendViaTypeAndTrait {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        trait LenFun extends PureFun[String,Int]
        object LenFunImpl extends LenFun {
          override def apply(s: String) = s.length
        }
        val lf = LenFunImpl
        assertPure(purityOf(lf("Test")))
        """) should compileAndRun
  }
  @Test def sideEffectFreeExtendViaTypeAndTraitAsLenFun {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        trait LenFun extends PureFun[String,Int]
        object LenFunImpl extends LenFun {
          override def apply(s: String) = s.length
        }
        val lf: LenFun = LenFunImpl
        assertPure(purityOf(lf("Test")))
        """) should compileAndRun
  }
  @Test def sideEffectFreeExtendViaTypeAndTraitAsPureFun {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        trait LenFun extends PureFun[String,Int]
        object LenFunImpl extends LenFun {
          override def apply(s: String) = s.length
        }
        val lf: PureFun[String,Int] = LenFunImpl
        assertPure(purityOf(lf("Test")))
        """) should compileAndRun
  }
  @Test def sideEffectFreeExtendViaTypeAndTraitAsFun {
    code("""
        trait Fun[I,O] {
          def apply(in: I): O
        }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        trait LenFun extends PureFun[String,Int]
        object LenFunImpl extends LenFun {
          override def apply(s: String) = s.length
        }
        val lf: Fun[String,Int] = LenFunImpl
        assertImpure(purityOf(lf("Test")))
        """) should compileAndRun
  }

  //TODO Type inference does not yet handle annotations correctly
  //  @Test def typeParamFunExecutionWithPureFun {
  //    code("""
  //        trait Fun[I,O] { def apply(in: I): O }
  //        class FunExec[I,O](fun: Fun[I,O], in: I) { @impureIf('O) def exec = fun(in) }
  //        type PureFun[I,O] = Fun[I,O @sideEffectFree]
  //        object LenFun extends PureFun[String,Int] { override def apply(in: String) = in.length }
  //
  //        val fb = new FunExec(LenFun, "Test")
  //        assertPure(purityOf(fb.exec))
  //        """) should compileAndRun
  //  }
  @Test def typeParamFunExecutionWithPureFunAndExplicitParams {
    code("""
        trait Fun[I,O] { def apply(in: I): O }
        class FunExec[I,O](fun: Fun[I,O], in: I) { @impureIf('O) def exec = fun(in) }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        object LenFun extends PureFun[String,Int] { override def apply(in: String) = in.length }

        val fb = new FunExec[String,Int @sideEffectFree](LenFun, "Test")
        assertPure(purityOf(fb.exec))
        """) should compileAndRun
  }

  @Test def typeParamFunExecutionWithImpureFun {
    code("""
        trait Fun[I,O] { def apply(in: I): O }
        class FunExec[I,O](fun: Fun[I,O], in: I) { @impureIf('O) def exec = fun(in) }
        type PureFun[I,O] = Fun[I,O @sideEffectFree]
        object LenFun extends Fun[String,Int] { @impure override def apply(in: String) = in.length }

        val fb = new FunExec(LenFun, "Test")
        assertImpure(purityOf(fb.exec))
        """) should compileAndRun
  }

}