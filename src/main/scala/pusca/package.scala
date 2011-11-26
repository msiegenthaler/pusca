import scala.annotation.TypeConstraint

package object pusca {
  type PureFunction1[T1, R] = Function1[T1, R] @__purefun
  type ->[T1, R] = PureFunction1[T1, R]

  /** removes the @sideEffect annotation */
  @inline def applySideEffect[A](a: A @sideEffect): A = a

  /** adds an @sideEffect annotation */
  @inline def addSideEffect[A](a: A): A @sideEffect = a

  /** INTERNAL: just a marker for communication between the compiler steps */
  def __internal__markReturnValue[A](a: A): A @__internal__returned = throw new UnsupportedOperationException
  def __internal__markReturnValueWithSideEffect[A](a: A): A @__internal__returned @sideEffect = throw new UnsupportedOperationException
  class __internal__returned extends StaticAnnotation with TypeConstraint

  /** INTERNAL: Marker for return values of pure functions */
  class __purefun extends StaticAnnotation with TypeConstraint
}