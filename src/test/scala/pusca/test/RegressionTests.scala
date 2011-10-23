package pusca.test

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
  classOf[ConstructorTests], //
  classOf[SideEffectAnnotationTest]))
class RegressionTests {
}