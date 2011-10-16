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

/**
 * The pureness of the method depends on the type parameter of the method and the surrounding class. An example
 * is <code>Function2#apply(a: A): B</code>, which may be pure or impure depending on B.<br/>
 * Usually there is no need to use this annotation, since it's the default for pureness.
 */
class dependentPure extends PurenessAnnotation

/** The method is declared as impure. */
class impure extends PurenessAnnotation

/**
 * Applicable on types that are used as return type to express the along with the type a side effect is caused
 * by the method (or the method depends on a side effect
 */
class sideEffect extends StaticAnnotation with TypeConstraint

/** Implementation detail, used by the compiler plugin to specify dependentPure further. */
private[pusca] class purenessDependsOn(params: List[Symbol]) extends StaticAnnotation
