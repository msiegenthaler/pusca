package pusca.test.old

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

@RunWith(classOf[Suite])
@SuiteClasses(Array( //
  classOf[PurityDefinitionTest], //
  classOf[AssignmentTest], //
  classOf[ImpurityTest], //
  classOf[PurityTest], //
  classOf[HigherLevelFunctionsTest], //
  classOf[ConstructorTest], //
  classOf[OverrideTest], //
  classOf[ParametrizedTypesTest], //
  classOf[TailcallsTest], //
  classOf[WildcardTest], //
  classOf[FunctionTypeTest], //
  classOf[SideEffectAnnotationTest]))
class RegressionTests {
}