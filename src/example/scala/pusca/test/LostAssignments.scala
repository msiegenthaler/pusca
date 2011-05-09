package pusca.test

object LostAssignments {
  object IOC {
    trait IO[A] {
      def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {}
    }
    def println(s: String) = {
      val a = 1
      new IO[Unit] {}
    }
    def readln = new IO[String] {}
  }
  import IOC._

  val x = println("Huhu")

  def main(arg: Array[String]): IO[Unit] = {
    println("One") //warn 1
    println("Two") //warn 2
    readln //warn 3
    val a = doSome flatMap ((_: Unit) => doSome2("Three"))
    doSome //warn 4
    doSome2("Four") //warn 5
    println("Five")
  }

  def doSome: IO[Unit] = println("Six")
  def doSome2(v: String) = {
    val i = 1
    println(v)
  }

  def doSome3(v: String) = v match {
    case "Hi" => println("Hello")
    case other => println(other)
  }
  def doSome4(v: String) = {
    v match {  // warn 6
      case "Hi" => println("Hello")
      case other => println(other)
    }
    println("huhu")
  }
  def doSome5(v: String) = v match {
    case "Hi" =>
      println("Hello") //warn 7
      "ha"
    case _ => "ho"
  }

  object X {
    //no warnings because <init>X is impure
    Predef.println("xx")
    2
  }

  1 //warn 8 (last)
}
