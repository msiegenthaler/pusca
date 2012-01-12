package pusca

import scala.annotation.TypeConstraint

/**
 * DO NOT USE. Internals of pusca that get added processed by the compiler plugin.
 */
object Internal {
  /** Gets the purity of the 'block' f */
  def purityOf[A](f: ⇒ A): Purity = throw new UnsupportedOperationException //replaced by the compiler

  /** Purity of a method */
  sealed trait Purity
  /** The method is always pure (regardless of the type parameters) */
  case object AlwaysPure extends Purity
  /** The method is always impure */
  case object AlwaysImpure extends Purity
  /** The method is always pure by declaration of the programmer */
  case object DeclaredPure extends Purity
  /** The method is impure if one of the type parameters in tparams is not pure */
  case class ImpureDependingOn(tparams: Set[Symbol]) extends Purity

  //  def markInfere[A](a: A): A @returnedInfere = throw new UnsupportedOperationException
  //  def markSideEffectFree[A](a: A): A @sideEffectFree = throw new UnsupportedOperationException

  //  class returnedInfere extends StaticAnnotation with TypeConstraint
}