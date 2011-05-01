package pusca

/**
 * Marks a class or method as not pure, meaning it has side-effects. Calling the function twice with the same
 * parameters might yield different results.
 */
class impure extends StaticAnnotation
