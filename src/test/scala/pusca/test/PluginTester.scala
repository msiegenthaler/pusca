package pusca.test

import annotation._
import java.io._
import scala.tools.nsc._
import scala.tools.nsc.settings._
import scala.tools.nsc.interpreter._
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import pusca.plugin._

object PluginTester {
  def apply(code: String) = new PluginTester().fromString(code).run

  class CompilesMatcher extends Matcher[PluginTestResult] {
    override def apply(r: PluginTestResult) = {
      val success = r.compiled && r.warnings.isEmpty && r.exception.isEmpty
      if (!success) println(r.out)
      MatchResult(success,
        "The compilation failed: " + (r.compileErrors :: r.warnings :: r.exception.map(e ⇒ List(e.toString)).getOrElse(Nil)).mkString(", "),
        "The compilation did not fail")
    }
  }
  class CompileErrorMatcher(expect: List[String]) extends Matcher[PluginTestResult] {
    override def apply(r: PluginTestResult) = {
      val expectedButNotFound = expect.filterNot(e ⇒ r.compileErrors.find(_.contains(e)).isDefined)
      val unexpected = r.compileErrors.filterNot(e ⇒ expect.find(e.contains(_)).isDefined)
      val errors = List(
        (if (r.exception.isDefined) "Exception during compilation: " + r.exception.get else ""),
        (if (!expectedButNotFound.isEmpty) "Missing compilation errors: " + expectedButNotFound.mkString(", ") else ""),
        (if (!unexpected.isEmpty || !r.warnings.isEmpty) "Unexpected compilation errors/warnings: " + (unexpected ::: r.warnings).mkString(", ") else "")).mkString(". ")
      val success = r.warnings.isEmpty && expectedButNotFound.isEmpty && unexpected.isEmpty && r.exception.isEmpty
      if (!success) println(r.out)
      MatchResult(success, errors, "NOT " + errors)
    }
  }
  class CompileWarnsMatcher(expect: List[String]) extends Matcher[PluginTestResult] {
    override def apply(r: PluginTestResult) = {
      val expectedButNotFound = expect.filterNot(e ⇒ r.warnings.find(_.contains(e)).isDefined)
      val unexpected = r.warnings.filterNot(e ⇒ expect.find(e.contains(_)).isDefined)
      val errors = List(
        (if (r.exception.isDefined) "Exception during compilation: " + r.exception.get else ""),
        (if (!expectedButNotFound.isEmpty) "Missing compilation warning: " + expectedButNotFound.mkString(", ") else ""),
        (if (!unexpected.isEmpty || !r.compileErrors.isEmpty) "Unexpected compilation errors/warnings: " + (r.compileErrors ::: unexpected).mkString(", ") else "")).mkString(". ")
      val success = r.compileErrors.isEmpty && expectedButNotFound.isEmpty && unexpected.isEmpty && r.exception.isEmpty
      if (!success) println(r.out)
      MatchResult(success, errors, "NOT " + errors)
    }
  }

  def code(code: String) = this(code)
  def yieldCompileError(errors: String*) = new CompileErrorMatcher(errors.toList)
  def warn(warns: String*) = new CompileWarnsMatcher(warns.toList)
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

  private def jarForClass(a: Class[_]) = {
    val classFile = "/" + a.getName.replace(".", "/") + ".class"
    val res = getClass.getResource(classFile).toString
    val r = if (res.startsWith("file:")) res.drop("file:".length).dropRight(classFile.length)
    else if (res.startsWith("jar:")) res.drop("jar:file:".length).dropRight(classFile.length() + 1)
    else throw new RuntimeException("Cannot find path for " + res)
    r
  }

  //the jar with all the plugin classes inside.
  lazy val pluginJar = {
    import java.util.jar._
    import java.io._
    val file = File.createTempFile("archive", ".jar")
    val fos = new FileOutputStream(file)
    val jos = new JarOutputStream(fos, new Manifest)
    val buffer = new Array[Byte](1024)

    def processDir(path: String, dir: File) {
      dir.listFiles.foreach { f ⇒
        if (f.isDirectory) {
          val np = path + f.getName + "/"
          jos.putNextEntry(new JarEntry(np))
          processDir(np, f)
        } else if (f.isFile) processFile(path, f)
      }
    }
    @tailrec def copy(in: InputStream, out: OutputStream) {
      val read = in.read(buffer, 0, buffer.length)
      if (read != -1) {
        out.write(buffer, 0, read)
        copy(in, out)
      } else ()
    }
    def processFile(path: String, file: File) {
      jos.putNextEntry(new JarEntry(path + file.getName))
      val fis = new FileInputStream(file)
      copy(fis, jos)
      fis.close
    }

    val dir = new File(jarForClass(classOf[PurePlugin]))
    processDir("", dir)

    jos.close
    file.deleteOnExit
    file
  }
  
  def interpreter(out: PrintWriter) = {
      val s = new Settings()
      s.usejavacp.value = false
      s.require.appendToValue("pure")

      s.classpath.value = ""
      s.classpath.append(jarForClass(classOf[List[_]])) //library
      s.classpath.append(jarForClass(classOf[IMain])) //compiler
      s.classpath.append(jarForClass(classOf[PurePlugin]))

      //plugin classes must be inside a jar
      s.plugin.appendToValue(pluginJar.getAbsoluteFile.toString)

      new IMain(s, out)
  }

  def run = {
    val out = new StringWriter
    try {
      val main = interpreter(new PrintWriter(out))

      val cs = code.reverse.map(_ + "\n()").map(main.interpret).filterNot(_ == Results.Success).map(_.toString)

      val outLines = out.toString.split("\n").toList.map { s ⇒ s.dropRight(s.reverse.takeWhile(_ == '\n').length) }
      val errors = outLines.
        filter(s ⇒ s.startsWith("<console>:") || s.startsWith("[init]")).
        filter(_.contains("error: "))
      val warnings = outLines.
        filter(_.startsWith("<console>:")).
        filter(_.contains("warning: "))

      PluginTestResult(out.toString, None, if (errors.isEmpty) cs else errors, warnings)
    } catch {
      case e ⇒
        e.printStackTrace
        PluginTestResult(out.toString, Some(e), Nil, Nil)
    }
  }
}

case class PluginTestResult(out: String, exception: Option[Throwable], compileErrors: List[String], warnings: List[String]) {
  def compiled = compileErrors.isEmpty
  def notcompiled = !compiled
  def hasError(text: String) = !compileErrors.filter(_.contains(text)).isEmpty
  def problems = compileErrors ::: warnings
}