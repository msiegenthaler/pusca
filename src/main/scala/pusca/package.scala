
package object pusca {

  /** removes the @sideEffect annotation */
  implicit def applySideEffect[A](a: A @sideEffect): A = a.asInstanceOf[A]
  
}