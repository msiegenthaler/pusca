package pusca

import scala.annotation.TypeConstraint

/**
 * DO NOT USE. Internals of pusca that get added processed by the compiler plugin.
 */
object Internal {
  def markInfere[A](a: A): A @returnedInfere = throw new UnsupportedOperationException
  def markSideEffect[A](a: A): A @returnedSideEffect @sideEffect = throw new UnsupportedOperationException
  def markSideEffectFree[A](a: A): A @returnedSideEffectFree @sideEffectFree = throw new UnsupportedOperationException

  class returnedInfere extends StaticAnnotation with TypeConstraint
  class returnedSideEffect extends StaticAnnotation with TypeConstraint
  class returnedSideEffectFree extends StaticAnnotation with TypeConstraint
}