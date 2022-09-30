package arcadia.view

import scala.xml._
import org.fusesource.scalate.TemplateSource
import arcadia.context._

/*
 * @since   Sep. 10, 2022
 * @version Sep. 10, 2022
 * @author  ASAMI, Tomoharu
 */
trait TemplateEngine {
  def isAccept(p: TemplateSource): Boolean
  def shutdown(): Unit
  def layoutAsNodes(template: TemplateSource, bindings: Map[String, Object]): NodeSeq
}

object TemplateEngine {
  trait Factory {
    def create(platform: PlatformContext): TemplateEngine
  }
}
