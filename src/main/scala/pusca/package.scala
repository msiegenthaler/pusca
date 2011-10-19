
package object pusca {

  /** removes the @sideEffect annotation */
  def applySideEffect[A](a: A @sideEffect): A = a.asInstanceOf[A]
  
}