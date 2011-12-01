import scala.annotation.TypeConstraint

package object pusca {
  /** The type A in a form that does not allow @sideEffect to be used on it */
  type Pure[A] = A @sef

  /** removes the @sideEffect annotation */
  @inline def applySideEffect[A](a: A @sideEffect): A = a

  /** adds an @sideEffect annotation */
  @inline def addSideEffect[A](a: A): A @sideEffect = a

  /** INTERNAL: just a marker for communication between the compiler steps */
  def __internal__markReturnValue[A](a: A): A @__internal__returned = throw new UnsupportedOperationException
  def __internal__markReturnValueWithSideEffect[A](a: A): A @__internal__returned @sideEffect = throw new UnsupportedOperationException
  class __internal__returned extends StaticAnnotation with TypeConstraint
}