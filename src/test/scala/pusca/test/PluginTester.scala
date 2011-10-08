package pusca.test

import annotation._
import java.io._
import scala.tools.nsc._
import scala.tools.nsc.settings._
import scala.tools.nsc.interpreter._
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

object PluginTester {
  def apply(code: String) = new PluginTester().fromString(code).run

  class CompilesMatcher extends Matcher[PluginTestResult] {
    override def apply(r: PluginTestResult) = {
      MatchResult(r.compiled, "The compilation failed: " + r.compileErrors, "The compilation did not fail")
    }
  }
  class CompileErrorMatcher(expect: List[String]) extends Matcher[PluginTestResult] {
    override def apply(r: PluginTestResult) = {
      val expectedButNotFound = expect.filterNot(e ⇒ r.compileErrors.find(_.contains(e)).isDefined)
      val unexpected = r.compileErrors.filterNot(e ⇒ expect.find(e.contains(_)).isDefined)
      val errors = List(
        (if (!expectedButNotFound.isEmpty) "Missing compilation errors: " + expectedButNotFound else ""),
        (if (!unexpected.isEmpty) "Unexpected compilation errors: " + unexpected else "")).mkString(". ")
      MatchResult(expectedButNotFound.isEmpty && unexpected.isEmpty, errors, "NOT " + errors)
    }
  }

  def yieldCompileError(errors: String*) = new CompileErrorMatcher(errors.toList)
  val compile = new CompilesMatcher
}

class PluginTester {
  val code: List[String] = "import pusca._" :: Nil

  def fromClasspath(n: String) = {
    val is = classOf[PluginTester].getResourceAsStream(n)
    if (is == null) throw new IOException("Classpath ressource " + n + " not found")
    val c = read(is)
    val cs = c :: code
    new PluginTester { override val code = cs }
  }

  def fromString(s: String) = {
    val cs = s :: code
    new PluginTester { override val code = cs }
  }

  private def read(is: java.io.InputStream) = {
    val r = new InputStreamReader(is)
    @tailrec def doRead(soFar: String): String = {
      val ca = new Array[Char](100)
      val c = r.read(ca)
      if (c != -1) {
        val s = new String(ca, 0, c)
        doRead(soFar + s)
      } else soFar
    }
    doRead("")
  }

  def run = {
    val out = new StringWriter
    try {
      val s = new Settings()
      s.embeddedDefaults[String]
      s.embeddedDefaults[pusca.pure]
      s.usejavacp.value = true
      s.require.appendToValue("pure")
      //point to the compiled classfiles
      //s.pluginsDir.value = (new File("target/scala-2.9.1/classes").getAbsoluteFile.toString + "/") //sbt
      s.pluginsDir.value = (new File(".target/scala-2.9.1/classes").getAbsoluteFile.toString + "/") //eclipse
      //must be inside a jar
      s.plugin.appendToValue(new File("src/test/pusca-descriptor.jar").getAbsoluteFile.toString)

      val main = new IMain(s, new PrintWriter(out))

      val cs = code.reverse.map(_ + "\n()").map(main.interpret).filterNot(_ == Results.Success).map(_.toString)

      val outLines = out.toString.split("\n").toList.map { s ⇒ s.dropRight(s.reverse.takeWhile(_ == '\n').length) }
      val errors = outLines.
        filter(s ⇒ s.startsWith("<console>:") || s.startsWith("[init]")).
        filter(_.contains("error: "))
      val warnings = outLines.
        filter(_.startsWith("<console>:")).
        filter(_.contains("warning: "))

      println(out)

      PluginTestResult(out.toString, None, if (errors.isEmpty) cs else errors, warnings)
    } catch {
      case e ⇒ PluginTestResult(out.toString, Some(e), Nil, Nil)
    }
  }
}

case class PluginTestResult(out: String, exception: Option[Throwable], compileErrors: List[String], warnings: List[String]) {
  def compiled = compileErrors.isEmpty
  def notcompiled = !compiled
  def hasError(text: String) = !compileErrors.filter(_.contains(text)).isEmpty
  def problems = compileErrors ::: warnings
}