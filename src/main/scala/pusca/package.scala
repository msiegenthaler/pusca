import scala.annotation.TypeConstraint

package object pusca {

  /** removes the @sideEffect annotation */
  def applySideEffect[A](a: A @sideEffect): A = a.asInstanceOf[A]

  /** adds an @sideEffect annotation */
  def addSideEffect[A](a: A): A @sideEffect = a

  /** INTERNAL: just a marker */
  //TODO maybe make private?
  def markReturnValue[A](a: A): A @returned = a
  def markReturnValueWithSideEffect[A](a: A): A @returned @sideEffect = a
  class returned extends StaticAnnotation with TypeConstraint

}