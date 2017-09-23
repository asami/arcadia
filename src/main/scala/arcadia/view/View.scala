package arcadia.view

import scala.xml.NodeSeq
import java.net.URL
import org.fusesource.scalate._
import org.goldenport.exception.RAISE
import org.goldenport.Strings
import com.asamioffice.goldenport.io.UURL
import arcadia._
import arcadia.context._
import arcadia.model.Model
import org.goldenport.bag.{ChunkBag, UrlBag}
import org.goldenport.util.{StringUtils, UrlUtils}

/*
 * @since   Jul. 15, 2017
 *  version Aug. 30, 2017
 * @version Sep. 23, 2017
 * @author  ASAMI, Tomoharu
 */
abstract class View() {
  def guard: Guard
  def apply(engine: ViewEngine, parcel: Parcel): Content
  def render(engine: ViewEngine, parcel: Parcel): NodeSeq
  def gv: (Guard, View) = (guard, this)
}

abstract class TemplateViewBase(template: TemplateSource) extends View() {
  def apply(engine: ViewEngine, parcel: Parcel): Content = {
    val bindings = _build_bindings(engine, parcel)
    val s = engine.layout(template, bindings)
    StringContent(MimeType.text_html, None, s)
  }

  def render(engine: ViewEngine, parcel: Parcel): NodeSeq = {
    val bindings = _build_bindings(engine, parcel)
    engine.render(template, bindings)
  }

  private def _build_bindings(engine: ViewEngine, parcel: Parcel): Map[String, AnyRef] = {
    val strategy0 = parcel.render getOrElse PlainHtml
    val strategy = strategy0.withContext(engine, parcel)
    _model_bindings(strategy, parcel) ++ property_Bindings(strategy)
  }

  private def _model_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] =
    parcel.getEffectiveModel.map(model_bindings(strategy, _)) getOrElse Map.empty

  protected def model_bindings(strategy: RenderStrategy, model: Model): Map[String, AnyRef] =
    model.viewBindings(strategy)

  protected def property_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty
}

case class TemplateView(
  guard: Guard,
  template: TemplateSource,
  properties: Map[String, AnyRef]
) extends TemplateViewBase(template) {
}

case class IndexView(template: TemplateSource) extends TemplateViewBase(template) {
  val guard = CommandGuard(classOf[IndexCommand])
}

case class EntityDetailView(template: TemplateSource) extends TemplateViewBase(template) {
  val guard = CommandGuard(classOf[EntityDetailCommand])
}

case class EntityListView(template: TemplateSource) extends TemplateViewBase(template) {
  val guard = CommandGuard(classOf[EntityListCommand])
}

case class DashboardView(template: TemplateSource) extends TemplateViewBase(template) {
  val guard = DashboardModelGuard
}

case class ModelView(template: TemplateSource) extends TemplateViewBase(template) {
  val guard = AllModelGuard
}

case class PageView(name: String, template: TemplateSource) extends TemplateViewBase(template) {
  val guard = OperationNameGuard(name)
}

case class HtmlView(url: URL, pathname: Option[String] = None) extends View() {
  private val _pathname = pathname getOrElse UrlUtils.takeLeafName(url)
  val guard = PathnameGuard(_pathname)

  def apply(engine: ViewEngine, parcel: Parcel): Content =
    StringContent(MimeType.text_html, None, new UrlBag(url).toText) // UTF-8

  def render(engine: ViewEngine, parcel: Parcel): NodeSeq = RAISE.notImplementedYetDefect
}

case class MaterialView(baseUrl: URL) extends View() {
  val guard = new Guard {
    def isAccept(p: Parcel): Boolean = p.command.fold(false) {
      case MaterialCommand(pathname) =>
        val url = new URL(baseUrl, pathname)
        UrlUtils.isExist(url)
      case _ => false
    }
  }

  def apply(engine: ViewEngine, parcel: Parcel): Content = {
    val c = parcel.takeCommand[MaterialCommand]
    val mime = {
      val a = for {
        suffix <- StringUtils.getSuffix(c.pathname)
        context <- parcel.context
        mime <- context.getMimetypeBySuffix(suffix)
      } yield mime
      a.getOrElse(MimeType.application_octet_stream)
    }
    val url = new URL(baseUrl, c.pathname)
    BinaryContent(mime, new UrlBag(url))
  }

  def render(engine: ViewEngine, parcel: Parcel): NodeSeq = RAISE.notImplementedYetDefect
}
// case class MaterialView(url: URL, pathname: Option[String] = None) extends View() {
//   private val _pathname = pathname getOrElse UrlUtils.takeLeafName(url)
//   val guard = PathnameGuard(_pathname)

//   val mimetype = MimeType.text_html // TODO

//   def apply(engine: ViewEngine, parcel: Parcel): Content =
//     BinaryContent(MimeType.text_html, new UrlBag(url))

//   def render(engine: ViewEngine, parcel: Parcel): NodeSeq = RAISE.notImplementedYetDefect
// }

case class AssetView(baseUrl: URL) extends View() {
  val guard = new Guard {
    def isAccept(p: Parcel): Boolean = p.command.fold(false) {
      case AssetsCommand(pathname) =>
        val url = new URL(baseUrl, pathname)
        UrlUtils.isExist(url)
      case _ => false
    }
  }

  def apply(engine: ViewEngine, parcel: Parcel): Content = {
    val c = parcel.takeCommand[AssetsCommand]
    val mime = {
      val a = for {
        suffix <- StringUtils.getSuffix(c.pathname)
        context <- parcel.context
        mime <- context.getMimetypeBySuffix(suffix)
      } yield mime
      a.getOrElse(MimeType.application_octet_stream)
    }
    val url = new URL(baseUrl, c.pathname)
    BinaryContent(mime, new UrlBag(url))
  }

  def render(engine: ViewEngine, parcel: Parcel): NodeSeq = RAISE.notImplementedYetDefect
}
object AssetView {
  def fromHtmlFilenameOrUri(p: String): AssetView = {
    val url = UURL.getURLFromFileOrURLName(p)
    AssetView(url)
  }
}

case class LayoutView(template: TemplateSource) extends TemplateViewBase(template) {
  val guard = NotImplementedYetGuard
}

case class PartialView(template: TemplateSource) extends TemplateViewBase(template) {
  val guard = NotImplementedYetGuard
}

case class ComponentView(guard: Guard, template: TemplateSource) extends TemplateViewBase(template) {
}
object ComponentView {
  // val RESOURCE_LIST = "resource_list"
  // val RESOURCE_DETAIL = "resource_detail"
  // val RECORD_LIST = "record_list"
  // val RECORD_DETAIL = "record_detail"

  def create(name: String, template: TemplateSource): ComponentView =
    ComponentView(ModelNameGuard(name), template)
}
