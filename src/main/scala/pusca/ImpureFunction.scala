package pusca

trait ImpureFunction0[@specialized +R] extends AnyRef { self =>
  @impure
  def apply(): R

  override def toString() = "<impurefunction0>"
}

trait ImpureFunction1[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) -T1, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends AnyRef { self =>
  @impure
  def apply(v1: T1): R
  //TODO compose, andThen

  override def toString() = "<impurefunction1>"
}

trait ImpureFunction2[@specialized(scala.Int, scala.Long, scala.Double) -T1, @specialized(scala.Int, scala.Long, scala.Double) -T2, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends AnyRef { self =>
  @impure
  def apply(v1: T1, v2: T2): R
  //TODO curried, tupled
  override def toString() = "<impurefunction2>"
}

//TODO 2..20