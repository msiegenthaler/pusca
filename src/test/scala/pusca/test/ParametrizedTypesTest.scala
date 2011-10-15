package pusca.test

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import PluginTester._
import pusca._
import scala.annotation.StaticAnnotation

class ParametrizedTypesTest extends JUnitSuite with ShouldMatchersForJUnit {
  
  
  trait Function[A,B] {
    @dependentPure
    @purenessDependsOn('B :: Nil)
    def apply(a: A): B
  }
  
  class Box[A](content: A) {
    @dependentPure
    @purenessDependsOn('B :: Nil)
    def map[B](f: Function[A,B]): Box[B] = {
      val b = f(content)
      new Box(b) 
    }
  }
  
  val box = new Box("Hi")
  val f = new Function[String,Int] {
    override def apply(a: String) = a.length
  }
  val lb = box.map(f)
  
}