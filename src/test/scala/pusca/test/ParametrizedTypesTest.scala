package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._
import pusca._
import scala.annotation.StaticAnnotation

class ParametrizedTypesTest extends JUnitSuite with ShouldMatchersForJUnit {

  trait Function[A, B] {
    @impureIf('B)
    def apply(a: A): B
  }

  class Box[A](content: A) {
    @impureIf('B)
    def map[B](f: Function[A, B]): Box[B] = {
      val b = f(content)
      new Box(b)
    }
  }

  val box = new Box("Hi")

  val f = new Function[String, Int] {
    override def apply(a: String) = a.length
  }
  val fi = new Function[String, Int @sideEffect] {
    override def apply(a: String) = {
      println(a)
      a.length
    }
  }

  //pure
  val lb = box.map(f)
  //impure
  val lb_ip = box.map(fi)

}