package pusca.plugin

import java.io.Reader
import java.io.InputStream
import java.io.FileNotFoundException

case class Config(key: List[String], value: String)

/**
 * Parses configuration for a file formatted like this example
 * <code>
 * scala {
 *  collection.immutable.List = Pure
 *  actors {
 *    Exit = Impure
 *  }
 *  annotation {
 *    default = Pure
 *    TypeConstraint = Impure
 *  }
 * }
 * </code>
 * that parses to
 * <ul>
 * <li>key: scala.collection.immutable.List, value: Pure</li>
 * <li>key: scala.actors.Exit, value: Impure</li>
 * <li>key: scala.annotation, value: Pure</li>
 * <li>key: scala.annotation.TypeConstraint, value: Impure</li>
 * </ul>
 */
object ConfigLoader {
  def loadFromUrl(url: java.net.URL): Option[Set[Config]] = {
    val is = try {
      url.openStream
    } catch {
      case f: FileNotFoundException ⇒ null
    }
    if (is == null) None
    else Some(ConfigLoader.load(is))
  }
  def load(from: String): Set[Config] = load(new java.io.StringReader(from))
  def load(from: Reader): Set[Config] = loadConfigFromHocon(from)
  def load(from: InputStream): Set[Config] = loadConfigFromHocon(new java.io.InputStreamReader(from, "UTF-8"))

  private def loadConfigFromHocon(from: Reader) = {
    import com.typesafe.config._
    import scala.collection.JavaConversions._
    val config = ConfigFactory.parseReader(from)
    config.entrySet.toList.flatMap { e ⇒
      val k = e.getKey.split('.').toList match {
        case "pusca" :: l if l.last == "default" ⇒ Some(l.dropRight(1))
        case "pusca" :: l                        ⇒ Some(l)
        case _                                   ⇒ None
      }
      k.flatMap { key ⇒
        e.getValue.unwrapped match {
          case s: String ⇒ Some(pusca.plugin.Config(key, s))
          case _         ⇒ None
        }
      }
    }.toSet
  }
}