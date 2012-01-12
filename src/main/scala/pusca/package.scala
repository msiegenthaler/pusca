import scala.annotation.TypeConstraint

package object pusca {
  /** The type A in a form that does not allow @sideEffect to be used on it */
  type Pure[+A] = A @sideEffectFree

  /** Function that evaluates without a side effect. */
  type PureFunction[-V1, +R] = Function1[V1, R @sideEffectFree]
  type ->[-V1, +R] = PureFunction[V1, R]
}