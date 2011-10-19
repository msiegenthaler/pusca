package pusca.test

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

@RunWith(classOf[Suite]) //</code> and <code>@SuiteClasses(TestClass1.class, ...)
@SuiteClasses(Array( //
  classOf[PurityDefinitionTest], //
  classOf[AssignmentTest], //
  classOf[ImpurityTest], //
  classOf[PurityTest], //
  classOf[SideEffectAnnotationTest]))
class RegressionTests {
}