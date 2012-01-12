package pusca

import scala.annotation.TypeConstraint

/** Annotation the defines the pureness of a method. The constructor is annotated on the class itself. */
sealed class PurenessAnnotation extends StaticAnnotation

/** The method is referential transparent and has no side effects. */
class pure extends PurenessAnnotation

/**
 * The method is referential transparent and without relevant side effects by declaration of the programmer.
 * The declaration is not verified by the compiler, the method may contain calls to impure methods.<br/>
 * It's appropriate to use this annotation on code that guarantees referential transparency but relies
 * on not-pure implementation details such as spawning multiple threads to parallelize the execution.
 */
class declarePure extends PurenessAnnotation

/** The method is declared as impure. */
class impure extends PurenessAnnotation

/**
 * The method is impure if any of the type parameters specified is a type annotated with @sideEffect.
 * Example:
 * <code>
 * 	trait Example[A] {
 * 		@impureIf('B)
 * 		def do[B](f: A => B): String = {...}
 * 		@impureIf('A)
 * 		def stuff(f: () => A): String = {...}
 *  }
 * </code>
 */
class impureIf(params: Symbol*) extends PurenessAnnotation

/**
 * The method is impure if its return type is a type annotated with @sideEffect. This annotation is the default
 * and should not be specified explicitly.
 * Example:
 * <code>
 * 	@impureIfReturnType def a[A,B](f: A => B): B = { ... } //equals @impureIf('B)
 * 	def a[A,B](f: A => B): B = { ... } //same as above
 * </code>
 */
class impureIfReturnType extends PurenessAnnotation

/**
 * Applicable on types that are used as return type to express the along with the type no side effect is caused
 * by the method and that the method does not depend on a side effect.
 */
class sideEffectFree extends PurenessAnnotation with TypeConstraint