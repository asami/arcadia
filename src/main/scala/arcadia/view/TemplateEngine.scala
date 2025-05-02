package arcadia.view

import scala.xml._
import java.net.URI
import org.fusesource.scalate.TemplateSource
import org.fusesource.scalate.util.FileResource
import org.fusesource.scalate.util.StringResource
import org.goldenport.io.InputSource
import org.goldenport.io.FileInputSource
import org.goldenport.io.StringInputSource
import arcadia.context._

/*
 * @since   Sep. 10, 2022
 * @version Mar. 21, 2025
 * @author  ASAMI, Tomoharu
 */
trait TemplateEngine {
  def isAccept(p: TemplateSource): Boolean
  def shutdown(): Unit
  def layoutAsNodes(template: TemplateSource, bindings: ViewEngine.Bindings): NodeSeq

  protected final def to_inputsource(p: TemplateSource): InputSource =
    p match {
      case m: FileResource => FileInputSource(m.file)
      case m: StringResource => StringInputSource(m.text, new URI(m.uri))
    }
}

object TemplateEngine {
  trait Factory {
    def create(platform: PlatformContext): TemplateEngine
  }
}
