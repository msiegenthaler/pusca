package pusca.plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class PurePlugin(val global: Global) extends Plugin {
  val name = "pure"
  val description = "Enforces purity in scala code."
  val components = List[PluginComponent]( //
    new MethodReturnTypeAnnotatorComponent(global),
    new PurityOfReplacerComponent(global))
}