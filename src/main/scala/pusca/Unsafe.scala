package pusca

/** Reserved for use inside the plugin. */
object Unsafe {

  def withoutSideEffect[A](a: A @sideEffect): A = a.asInstanceOf[A]

}