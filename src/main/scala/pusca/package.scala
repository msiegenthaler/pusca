import scala.annotation.TypeConstraint

package object pusca {

  /** removes the @sideEffect annotation */
  @inline def applySideEffect[A](a: A @sideEffect): A = a

  /** adds an @sideEffect annotation */
  @inline def addSideEffect[A](a: A): A @sideEffect = a

  /** INTERNAL: just a marker */
  //TODO maybe make private?
  def markReturnValue[A](a: A): A @returned = throw new UnsupportedOperationException
  def markReturnValueWithSideEffect[A](a: A): A @returned @sideEffect = throw new UnsupportedOperationException
  class returned extends StaticAnnotation with TypeConstraint

}