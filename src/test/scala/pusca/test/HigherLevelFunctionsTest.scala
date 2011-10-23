package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._

class HigherLevelFunctionsTest extends JUnitSuite with ShouldMatchersForJUnit {
  @Test def functionWithSideEffectIsNotCompatibleWithPure {
    code("""
        @pure def m(f: String => Int) = {
        	f("Hi") == 0
    		}
        @impure def p(s: String) = s.length
    		@pure def x = {
        	m(p _)
    		}
      """) should yieldCompileError("type mismatch")
  }
  
  @Test def functionWithSideEffectIsNotCompatibleWithPurePartialApplication {
  	code("""
  			@pure def m(f: String => Int) = {
  				f("Hi") == 0
  			}
  			@impure def p(i: Int)(s: String) = s.length * i
  			@pure def x = {
  				m(p(2) _)
  			}
  	""") should yieldCompileError("type mismatch")
  }
  @Test def functionWithSideEffectIsNotCompatibleWithPurePartialApplication2 {
  	code("""
  			@pure def m(f: String => Int) = {
  				f("Hi") == 0
  			}
  			@impure def p(i: Int, s: String) = s.length * i
  			@pure def x = {
  				m(p(2, _))
  			}
  	""") should yieldCompileError("type mismatch")
  }

  @Test def functionWithSideEffectCannotBeEvaluatedInsidePureFunctionLast {
    code("""
        @pure def m(f: String => Unit @sideEffect): Unit = f("Hi")
      """) should yieldCompileError("type mismatch")
  }

  @Test def functionWithSideEffectCannotBeEvaluatedInsidePureFunctionMiddle {
    code("""
        @pure def m(f: String => Unit @sideEffect): Unit = {
        	f("Hi")
        	()
    		}
      """) should yieldCompileError("impure method call inside the pure method 'm'")
  }

  @Test def functionWithSideEffectCanBePassedToPureFunction {
    code("""
        @pure def m(f: String => Unit @sideEffect): String = "Hi"
      """) should compile
  }
  
  @Test def functionWithSideEffectIsCompatibleWithImpure {
    code("""
        @impure def m(f: String => Int @sideEffect) = f("Hi")
        @impure def l(f: String) = f.length
        @impure def x = m(l _)
      """) should compile
  }
  @Test def functionWithSideEffectIsCompatibleWithPure {
  	code("""
  			@impure def m(f: String => Int @sideEffect) = f("Hi")
  			@pure def l(f: String) = f.length
  			@impure def x = m(l _)
  	""") should compile
  }

  @Test def functionWithTypeParameterIncludesPurenessOnPureWithClass {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  	    class LengthFun extends Function1[String,Int] { override def apply(s: String) = s.length }
  			@pure def x = m(new LengthFun)
  		""") should compile
  }
  @Test def functionWithTypeParameterIncludesPurenessOnPureWithAnon {
    code("""
        	def m[A](f: String => A): A = f("Hello")
          m(_ + "!")
        """) should compile
  }
  @Test def functionWithTypeParameterIncludesPurenessOnPureWithAnon2 {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@pure def x = m(_.length)
  		""") should compile
  }
  @Test def functionWithTypeParameterIncludesPurenessOnPureWithAnonLong {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@pure def x = m(x => x.length)
  		""") should compile
  }

	@Test def functionWithTypeParameterIncludesPurenessOnImpureWithClass1 {
		code("""
				def m[A](f: String => A): A = f("Hello")
				@impure def il(i: Int): Int @sideEffect = i + i
				class LengthFunI extends Function1[String,Int @sideEffect] { override def apply(s: String) = il(s.length) }
				@pure def x = m[Int @sideEffect](new LengthFunI)
		""") should yieldCompileError("method 'x' has @pure annotation and a @sideEffect return type")
	}
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithClass2 {
    code("""
  			def m[A](f: String => A): A = f("Hello")
        @impure def il(i: Int) = i + i
  	    class LengthFunI extends Function1[String,Int @sideEffect] { override def apply(s: String) = il(s.length) }
  			@pure def x = m[Int @sideEffect](new LengthFunI)
  		""") should yieldCompileError("method 'x' has @pure annotation and a @sideEffect return type")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithClass3 {
    code("""
  			def m[A](f: String => A): A = f("Hello")
        @impure def il(i: Int) = i + i
  	    class LengthFunI extends Function1[String,Int @sideEffect] { override def apply(s: String) = il(s.length) }
  			@pure def x = m(new LengthFunI)
  		""") should yieldCompileError("no type parameters for method m")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithClass4 {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: Int) = i + i
  			class LengthFunI extends Function1[String,Int @sideEffect] { override def apply(s: String) = il(s.length) }
  			@pure def x = {
  	    	m(new LengthFunI)
  	    	"Huhu"
  			}
  		""") should yieldCompileError("no type parameters for method m")
  }

  @Test def functionWithTypeParameterCalledFromImpureWithImpure {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			def il(i: String): Int @sideEffect = i.length
        @impure def x = {
        	val f: Function1[String, Long @sideEffect] = (s: String) => { 
        		il(s)
        		10L
    			} 
        	m[Long @sideEffect](f)
        	()
    		}
  		""") should compile
  }  
  @Test def functionWithTypeParameterCalledFromImpureWithImpure2 {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			def il(i: String): Int @sideEffect = i.length
        @impure def x = {
        	m[Int @sideEffect](s => il(s))
        	()
    		}
  		""") should compile
  }    
  
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonAndExplicit {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = m[Int @sideEffect](il _)
  		""") should yieldCompileError("method 'x' has @pure annotation and a @sideEffect return type")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonMiddleExplicit {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = {
        	m[Int @sideEffect](il _)
        	10
    		}
  		""") should yieldCompileError("impure method call inside the pure method 'x'")
  }

  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonAndUnitExplicit {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = ()
  			@pure def x = m[Unit @sideEffect](il _)
  		""") should yieldCompileError(" method 'x' has @pure annotation and a @sideEffect return type")
  }
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonAndUnitMiddleExplicit {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = ()
  			@pure def x = {
  				m[Unit @sideEffect](il _)
  				10
  			}
  		""") should yieldCompileError("impure method call inside the pure method 'x'")
  }
  
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithWrongType {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = {
        	m[Int](x => il(x))
        	10
    		}
  		""") should yieldCompileError("type mismatch")
  }
  
  @Test def functionWithTypeParameterIncludesPurenessOnImpureWithAnonLongExplicit {
    code("""
  			def m[A](f: String => A): A = f("Hello")
  			@impure def il(i: String) = i.length
  			@pure def x = m[Int @sideEffect](s => il(s))
  		""") should yieldCompileError("method 'x' has @pure annotation and a @sideEffect return type")
  }

  @Test def pureFunctionWithParameterThatIsNotAReturnValueAcceptsPure {
    code("""
        @pure def m[A](f: String => A): Unit = {
        	f("Huhu")
        	()
    		}
        @pure def x = m(_.length)
      """) should compile
  }
  
  @Test def pureFunctionWithParameterDeclaredSideEffectAcceptsPure {
    code("""
        @pure def m[A](f: String => A): Unit = ()
        @pure def x = m(_.length)
      """) should compile
  }
  @Test def pureFunctionWithParameterDeclaredSideEffectCannotEvaluateIt {
    code("""
        @pure def m[A](f: String => A): Unit = f("hi")
      """) should compile
  }

  @Test def pureFunMayComposeImpureOnes {
    code("""
        @pure def then(sf: Function0[Unit @sideEffect])(f: Function0[Unit @sideEffect]): Function0[Unit @sideEffect] = {
        	() => {
        		sf()
        		f()
    			}
    		}""") should compile
  }

  @Test def composingOfImpureFunsInsideAPureOne {
    code("""
        @impure def prt(s: String) = ()
        @pure def then(sf: Function0[Unit @sideEffect])(f: Function0[Unit @sideEffect]): Function0[Unit @sideEffect] = {
        	() => {
        		sf()
        		f()
        		()
    			}
    		}
        @pure def myProg = {
        	val f1 = () => prt("Hello")
    			val f2 = () => prt("Hello")
        	then(f1)(f2)
    		}
        """) should compile
  }

  @Test def composingOfImpureFunsInsideAPureOneAndExecuteInsideAnImpure {
    code("""
        @impure def prt(s: String) = ()
        @pure def then(sf: Function0[Unit @sideEffect])(f: Function0[Unit @sideEffect]): Function0[Unit @sideEffect] = {
        	() => {
        		sf()
        		f()
        		()
    			}
    		}
        @pure def myProg = {
        	val f1 = () => prt("Hello")
    			val f2 = () => prt("Hello")
        	then(f1)(f2)
    		}
        @impure def exec = myProg()
        """) should compile
  }
}