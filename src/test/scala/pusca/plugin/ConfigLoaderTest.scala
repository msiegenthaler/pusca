package pusca.plugin

import scala.annotation._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test

class ConfigLoaderTest extends JUnitSuite with ShouldMatchersForJUnit {
  private def load(s: String) = {
    @tailrec def sortKey(a: List[String], b: List[String]): Boolean = a match {
      case e :: ra ⇒ b match {
        case `e` :: rb        ⇒ sortKey(ra, rb)
        case f :: r2 if f > e ⇒ true
        case _                ⇒ false
      }
      case Nil ⇒ b.nonEmpty
    }
    def sorter(a: Config, b: Config) = sortKey(a.key, b.key)
    ConfigLoader.load(s).toList.sortWith(sorter)
  }

  @Test def emptyConfig {
    val config = load("")
    config should be(Nil)
  }

  @Test def loadSimpleKey {
    val config = load("a=b")
    config should be(Config("a" :: Nil, "b") :: Nil)
  }

  @Test def loadSimpleKeyWithSpaces {
    val config = load("a = b")
    config should be(Config("a" :: Nil, "b") :: Nil)
  }

  @Test def loadSimpleKeyWithSpacesBefore {
    val config = load("  a = b")
    config should be(Config("a" :: Nil, "b") :: Nil)
  }

  @Test def loadMultiCharacterKey {
    val config = load("hello=mario")
    config should be(Config("hello" :: Nil, "mario") :: Nil)
  }

  @Test def loadPeriodSeparatedKey {
    val config = load("ch.inventsoft.b.test=value")
    config should be(Config("ch" :: "inventsoft" :: "b" :: "test" :: Nil, "value") :: Nil)
  }

  @Test def loadKeyInsideGroup {
    val config = load("""
        ch {
          Test = Pure
        }""")
    config should be(Config("ch" :: "Test" :: Nil, "Pure") :: Nil)
  }
  @Test def loadKeyInsideGroupNoNewlines {
    val config = load("ch{Test=Pure}")
    config should be(Config("ch" :: "Test" :: Nil, "Pure") :: Nil)
  }

  @Test def loadKeyInsideGroupWithAValue {
    val config = load("""
        ch {
          default = Impure
          Test = Pure
        }""")
    config should be(Config("ch" :: Nil, "Impure") ::
      Config("ch" :: "Test" :: Nil, "Pure") ::
      Nil)
  }

  @Test def loadKeyInsideNestedGroup {
    val config = load("""
        ch {
          inventsoft {
            Test = Pure
          }
        }""")
    config should be(Config("ch" :: "inventsoft" :: "Test" :: Nil, "Pure") :: Nil)
  }
  @Test def loadKeyInsideNestedGroupNoNewlines {
    val config = load("ch { inventsoft { Test = Pure } }")
    config should be(Config("ch" :: "inventsoft" :: "Test" :: Nil, "Pure") :: Nil)
  }
  @Test def loadKeyInsideNestedGroupNoWhitespace {
    val config = load("ch{inventsoft{Test=Pure}}")
    config should be(Config("ch" :: "inventsoft" :: "Test" :: Nil, "Pure") :: Nil)
  }

  @Test def loadKeyInsideNestedGroupsWithValues {
    val config = load("""
        ch {
          default = Impure
          inventsoft {
            default = value
            Test = Pure
          }
        }""")
    config should be(Config("ch" :: Nil, "Impure") ::
      Config("ch" :: "inventsoft" :: Nil, "value") ::
      Config("ch" :: "inventsoft" :: "Test" :: Nil, "Pure") ::
      Nil)
  }

  @Test def loadExample {
    val config = load("""
      scala {
        collection.immutable.List = Pure
        actors {
          Exit = Impure
        }
        annotation {
          default = Pure
          TypeConstraint = Impure
        }
       }""")

    val expected = Config("scala" :: "actors" :: "Exit" :: Nil, "Impure") ::
      Config("scala" :: "annotation" :: Nil, "Pure") ::
      Config("scala" :: "annotation" :: "TypeConstraint" :: Nil, "Impure") ::
      Config("scala" :: "collection" :: "immutable" :: "List" :: Nil, "Pure") ::
      Nil
    config should be(expected)
  }
}