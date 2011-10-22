
package object pusca {

  /** removes the @sideEffect annotation */
  def applySideEffect[A](a: A @sideEffect): A = a.asInstanceOf[A]
  
  /** adds an @sideEffect annotation */
  def addSideEffect[A](a: A): A @sideEffect = a
}