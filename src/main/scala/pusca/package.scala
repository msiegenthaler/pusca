import scala.annotation.TypeConstraint

package object pusca {
  /** Any with side effect */
  type SideEffect = Any @sideEffect

  /** removes the @sideEffect annotation */
  @inline def applySideEffect[A](a: A @sideEffect): A = a

  /** adds an @sideEffect annotation */
  @inline def addSideEffect[A](a: A): A @sideEffect = a

  /** INTERNAL: just a marker for communication between the compiler steps */
  def __internal__markReturnValue[A](a: A): A @__internal__returned = throw new UnsupportedOperationException
  def __internal__markReturnValueWithSideEffect[A](a: A): A @__internal__returned @sideEffect = throw new UnsupportedOperationException
  class __internal__returned extends StaticAnnotation with TypeConstraint
}