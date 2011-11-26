import scala.annotation.TypeConstraint

package object pusca {
  type PureFunction1[-T1, +R] = Function1[T1, R] @pureFun
  type ->[-T1, +R] = PureFunction1[T1, R]
  type PureFunction2[-T1, -T2, +R] = Function2[T1, T2, R] @pureFun
  type PureFunction3[-T1, -T2, -T3, +R] = Function3[T1, T2, T3, R] @pureFun
  type PureFunction4[-T1, -T2, -T3, -T4, +R] = Function4[T1, T2, T3, T4, R] @pureFun
  type PureFunction5[-T1, -T2, -T3, -T4, -T5, +R] = Function5[T1, T2, T3, T4, T5, R] @pureFun
  type PureFunction6[-T1, -T2, -T3, -T4, -T5, -T6, +R] = Function6[T1, T2, T3, T4, T5, T6, R] @pureFun

  /** removes the @sideEffect annotation */
  @inline def applySideEffect[A](a: A @sideEffect): A = a

  /** adds an @sideEffect annotation */
  @inline def addSideEffect[A](a: A): A @sideEffect = a

  /** INTERNAL: just a marker for communication between the compiler steps */
  def __internal__markReturnValue[A](a: A): A @__internal__returned = throw new UnsupportedOperationException
  def __internal__markReturnValueWithSideEffect[A](a: A): A @__internal__returned @sideEffect = throw new UnsupportedOperationException
  class __internal__returned extends StaticAnnotation with TypeConstraint
}