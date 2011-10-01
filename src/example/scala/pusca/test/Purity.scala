package pusca.test

import pusca._

object Purity {

  //Simple pure functions
  def double(x: Int) = x * 2
  def sum(a: Int, b: Int) = a + b
  def sum(e: List[Int]) = e.foldLeft(0)(_ + _)

  //simple impure function
  @impure
  def sysout(a: String) = println(a)

  //impure functions may call pure and impurefunctions
  @impure
  def sysout_double(a: Int) = {
    val b = double(a)
    sysout(b.toString)
  }
  @impure
  def sysout_double_(a: Int) = sysout(double(a).toString)

  //pure functions may access vals
  val factor = 3
  def applyFactor(i: Int) = i * factor

  //access to vars in the same type
  var a = 0
  def number = a //error 1
  def setNumber(nr: Int) = a = nr // error 2
  @impure
  def number_ = a
  @impure
  def setNumber_(nr: Int) = a = nr

  //Access to vars in different types
  object A {
    var nr = 0
  }
  object B {
    def readANr = A.nr //error 3
    def writeANr(i: Int) = A.nr = i //error 4
    @impure
    def readANr_ = A.nr
    @impure
    def writeANr_(i: Int) = A.nr = i
  }

  //pure function must not call impure ones
  def sumAndPrint(a: Int, b: Int) = {
    sysout("" + a + " + " + b) // error 5
    a + b
  }
  @impure
  def sumAndPrint_(a: Int, b: Int) = {
    sysout("" + a + " + " + b)
    a + b
  }

  //declare pure makes impure stuff pure
  @declarePure
  def logged_sum(a: Int, b: Int) = {
    sysout("" + a + " + " + b)
    a + b
  }
  def logged_double(v: Int) = logged_sum(v, v)

  // overriding pure functions with a pure impl
  trait C {
    def sum(a: Int, b: Int): Int
  }
  object D extends C {
    override def sum(a: Int, b: Int) = a + b
  }

  // overriding pure functions with impure
  object E extends C {
    @impure
    override def sum(a: Int, b: Int) = { //error 6
      println(a + b) 
      a + b
    }
  }

  //impure constructors
  class F(var v: Int)
  def new_f(v: Int) = new F(v) //error 7
  @impure
  def new_f_(v: Int) = new F(v)

  // impure 'init' actions make constructor impure
  class G {
    println("hi")
  }
  def new_g = new G //error 8
  @impure
  def new_g_ = new G

  // impure constructors get inherited
  class H extends G
  def new_h = new H //error 9
  @impure
  def new_h_ = new H

  //impure constuctors get inherited by anonymous subclasses
  def new_g_subclass = new G { def a = 10 } //error 10
  @impure
  def new_g_subclass_ = new G { def a = 10 }

  //detects calls to impure library functions as impure
  def sysout_(a: String) = println(a) //error 11 (last)

}
