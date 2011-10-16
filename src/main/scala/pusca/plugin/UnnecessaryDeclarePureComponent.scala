package pusca.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

/**
 * Warns on unnecessary @delcarePure annotations. The annotation is not necessary if the method is pure anyway.
 */
class UnnecessaryDeclarePureComponent(val global: Global) extends PluginComponent with PuscaDefinitions {
  import global._

  val runsAfter = List("typer")
  val phaseName = "unnessesaryDeclarePure"
  def newPhase(prev: Phase) = new UnnessesaryDeclarePure(prev)

  class UnnessesaryDeclarePure(prev: Phase) extends StdPhase(prev) {
    private def isAnnotDeclarePure(s: DefDef) = hasAnnotation(s.symbol, Annotation.declarePure)
    override def name = phaseName

    override def apply(unit: CompilationUnit) {
      def handle(t: Tree): Unit = {
        t match {
          case d: DefDef if isAnnotDeclarePure(d) && !hasAnnotation(d.rhs.tpe, Annotation.sideEffect) ⇒
            reporter.warning(d.pos, "@declarePure is unnecessary, function '" + d.name + "' is pure")
          case _ ⇒ ()
        }
        t.children.foreach(handle)
      }
      handle(unit.body)
    }
  }
}