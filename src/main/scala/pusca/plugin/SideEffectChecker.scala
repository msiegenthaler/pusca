package pusca
package plugin

import scala.tools.nsc.Global

abstract class SideEffectChecker {
  val global: Global
  import global._

  object checker extends AnnotationChecker {
    private val sideEffectAnnotation = definitions.getClass("pusca.sideEffect")
    private def hasSideEffect(tpe: Type) = tpe.annotations.find(_.atp.typeSymbol == sideEffectAnnotation).isDefined

    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      val se1 = hasSideEffect(tpe1)
      val se2 = hasSideEffect(tpe2)
      se2 || !se1
    }
  }
}