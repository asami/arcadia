package arcadia.view

import scala.xml._
import org.fusesource.scalate.TemplateSource
import org.fusesource.scalate.util.{StringResource, Resource}
import org.goldenport.xml.XhtmlUtils
import arcadia.context._


/*
 * @since   Sep. 10, 2022
 *  version Sep. 10, 2022
 * @version Mar. 19, 2025
 * @author  ASAMI, Tomoharu
 */
case class TemplateEngineHangar(
  engines: Vector[TemplateEngine] = Vector.empty
) {
  def +(rhs: TemplateEngineHangar): TemplateEngineHangar = TemplateEngineHangar(engines ++ rhs.engines)

  def shutdown(): Unit =
    for (x <- engines)
      x.shutdown()

  def layoutAsNodes(template: TemplateSource, bindings: ViewEngine.Bindings): NodeSeq =
    engines.toStream.filter(_.isAccept(template)).headOption.
      map(_.layoutAsNodes(template, bindings)).
      getOrElse(_parse_html(template))

  private def _parse_html(template: TemplateSource): NodeSeq =
    template match {
      case m: StringResource => XhtmlUtils.parseFragmentNode(m.text)
      case m: Resource => XhtmlUtils.parseFragmentNode(m.text) // TODO encoding
    }
}

object TemplateEngineHangar {
  trait Factory {
    def create(platform: PlatformContext): TemplateEngineHangar
  }
  object Factory {
    val empty = new TemplateEngineHangar.Factory {
      def create(platform: PlatformContext): TemplateEngineHangar = TemplateEngineHangar.empty
    }

    case class SimpleFactory(
      factories: Vector[TemplateEngine.Factory]
    ) extends Factory {
      def create(platform: PlatformContext): TemplateEngineHangar =
        TemplateEngineHangar(factories.map(_.create(platform)))
    }

    def apply(p: TemplateEngine.Factory, ps: TemplateEngine.Factory*): TemplateEngineHangar.Factory =
      SimpleFactory((p +: ps).toVector)
  }

  val empty = TemplateEngineHangar()

  def apply(p: TemplateEngine, ps: TemplateEngine*): TemplateEngineHangar =
    TemplateEngineHangar((p +: ps).toVector)
}
  
