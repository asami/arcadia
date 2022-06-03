package arcadia.view

import scala.xml._
import org.fusesource.scalate._
import arcadia._
import arcadia.context._

/*
 * @since   May. 22, 2022
 * @version May. 22, 2022
 * @author  ASAMI, Tomoharu
 */
class ScalateTemplateEngine(val platform: PlatformContext) {
  private var _scalate = _create_scalate()

  private def _create_scalate() = {
    val scalate = new TemplateEngine()
    scalate.workingDirectory = platform.createTempDirectory()
    scalate.mode = "develop"
    scalate.allowReload = true // TODO
    scalate.importStatements = scalate.importStatements ::: List(
      "import arcadia.view._",
      "import arcadia.model._",
      "import arcadia.domain._"
    )
    scalate
  }

  private def _refresh() = {
    _scalate = _create_scalate()
  }

  def shutdown() = _scalate.shutdown()

  def layout(template: TemplateSource, bindings: Map[String, Object]): String = {
    val r = _scalate.layout(template, bindings)
    // if (false)
    //   _refresh()
    r
  }

  def layoutAsNodes(template: TemplateSource, bindings: Map[String, Object]): NodeSeq = {
    val r = _scalate.layoutAsNodes(template.uri, bindings)
    // if (flase)
    //   _refresh()
    r
  }
}

object ScalateTemplateEngine {
}
