package pusca

/**
 * Used to mark a block (class or method) with impure content as pure.
 * Use with care since this can hurt the compiler-provided proof of side-effect freeness. <br/>
 * It's appropriate to use this annotation on code that guarantees referential transparency but relies 
 * on not-pure implementation details such as spawning multiple threads to parallelize the execution.
 */
class declarePure extends StaticAnnotation
