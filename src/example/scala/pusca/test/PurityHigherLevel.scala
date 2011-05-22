package pusca.test

import pusca._

object PurityHigherLevel {

  // passing around pure functions
  def sum(a: Int, b: Int) = a + b
  def exec(f: (Int, Int) => Int) = f(1, 2).toString
  def forall(a: List[Int], f: Int => Unit) = a.foreach(f)
  def sumList(a: List[Int]) = a.foldLeft(0)(sum _)
  def map[B](a: List[Int], f: Int => B) = a.map(f)

  //anonymous pure functions
  def anonExec(a: Int) = {
    val f = (a: Int) => a.toString
    f(a)
  }
  def sumListAnon(a: List[Int]) = {
    val f = (a: Int, b: Int) => a + b
    a.foldLeft(0)(f)
  }
  def sumListAnon_(a: List[Int]) = a.foldLeft(0)(_ + _)
  def sumListInline1(a: List[Int]) = {
    def f(a: Int, b: Int) = a + b
    a.foldLeft(0)(f)
  }

  //impure function cannot be used as a pure one
  @impure
  def impurePrint(a: String) = println("- " + a)
  def printAll(a: List[String]) = {
    a.foreach(impurePrint) //error 1
  }
  def printAll2(a: List[String]) = {
    a.foreach(impurePrint _) //error 2
  }
  @impure
  def impureSum(a: Int, b: Int) = a + b
  def execImpureSum: Unit = {
    impureSum(20, 10) //error 3
  }

  //a pure function must not execute an impure one
  def executeImpure(a: Int => String @impure) = a(10) //error 4
  def boxedImpure(a: Some[Int => String @impure]) = a match {
    case Some(a) => a(10) //error 5
    case _ => ""
  }
  def boxedImpure2(a: Some[Int => String @impure]) = a.foreach(_(10)) //error 6

  //a pure function may operate on impure if it does not execute them 
  def passAroundImpure(a: Int => String @impure, b: String => Boolean @impure): Int => Boolean @impure = {
    @impure
    def f(i: Int) = {
      val j = a(i)
      b(j)
    }
    f
  }

  //Wrapping
  def referenceToImpure = impureSum _
  def wrapImpure1 = () => impureSum(10, 15)
  def wrapImpure2 = (a: Int) => impureSum(10, a)
  def wrapImpure3: Int => Int @impure = impureSum(_, 15)
  def callWrapped = {
    val x: Int = wrapImpure1() //error 7
    wrapImpure2(10) //error 8
  }
  def tryToWrapInPure: Int => Int = sum(_, 10)

  //assigning impure to pure
  def impureToPure(f: String => Unit @impure) = {
    val fa: String => Unit = f //error 9
    fa("Hi")
  }
  def impureToPure2(f: String => Unit @impure) = {
    val fa = new Function1[String, Unit] {
      override def apply(a: String) = f(a) //error 10
    }
    fa("Hi")
  }
  
  //execution of local impure
  def impureToImpure(f: String => Unit @impure) = {
    val fa: String => Unit @impure = f
    fa("Hi") //error 11 (last)
  }

  //casting impure to pure
  def castImpureToPure(f: String => Unit @impure) = {
    val fa: String => Unit = f.asInstanceOf[String => Unit] //class cast exception
    fa("Hi")
  }

  //assigning pure to impure
  def pureToImpure(f: String => Int): String => Int @impure = f
}
