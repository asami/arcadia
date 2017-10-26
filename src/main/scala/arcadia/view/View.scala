package arcadia.view

import scala.xml.NodeSeq
import java.net.URL
import org.fusesource.scalate._
import org.goldenport.exception.RAISE
import org.goldenport.Strings
import org.goldenport.record.v2._
import org.goldenport.bag.{ChunkBag, UrlBag}
import org.goldenport.io.UrlUtils
import org.goldenport.util.StringUtils
import com.asamioffice.goldenport.io.UURL
import arcadia._
import arcadia.context._
import arcadia.model._
import ViewEngine.{PROP_VIEW_SERVICE, PROP_VIEW_MODEL}

/*
 * @since   Jul. 15, 2017
 *  version Aug. 30, 2017
 *  version Sep. 30, 2017
 * @version Oct. 21, 2017
 * @author  ASAMI, Tomoharu
 */
abstract class View() {
  def guard: Guard
  def apply(engine: ViewEngine, parcel: Parcel): Content = 
    execute_apply(engine, parcel.forView(engine))
  def render(engine: ViewEngine, parcel: Parcel): NodeSeq =
    execute_apply(engine, parcel.forView(engine)).asXml
  def render(strategy: RenderStrategy): NodeSeq = strategy.viewContext.
    map(x => render(x.engine, x.parcel.withRenderStrategy(strategy))) getOrElse {
      RAISE.noReachDefect
    }
  def gv: (Guard, View) = (guard, this)

  protected def execute_apply(engine: ViewEngine, parcel: Parcel): Content =
    engine.eval(parcel, execute_Apply(engine, parcel))

  protected def execute_Apply(engine: ViewEngine, parcel: Parcel): Content
}

trait ModelViewBase[T <: Model] extends View {
  def model: T
  def guard: Guard = ModelNameGuard(model.featureName)
  protected def execute_Apply(engine: ViewEngine, parcel: Parcel): Content = {
    val p = parcel.forComponent(model)
    def s = _strategy(engine, p)
    engine.applyComponentOption(p) getOrElse model.apply(s)
  }

  private def _strategy(engine: ViewEngine, parcel: Parcel) = parcel.render.map(_.forComponent(engine, parcel)) getOrElse {
    RAISE.noReachDefect
  }
}

abstract class TemplateViewBase(template: TemplateSource) extends View() {
  protected def execute_Apply(engine: ViewEngine, parcel: Parcel): Content = {
    val bindings = _build_bindings(engine, parcel)
    XmlContent(engine.render(template, bindings))
  }

  private def _build_bindings(engine: ViewEngine, parcel: Parcel): Map[String, AnyRef] = {
    val strategy0 = parcel.render getOrElse PlainHtml
    val strategy = strategy0.withViewContext(engine, parcel)
    _model_bindings(strategy, parcel) ++ property_Bindings(strategy) ++ _service_bindings(strategy, parcel)
  }

  private def _service_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] =
    (parcel.context orElse strategy.viewContext.flatMap(_.parcel.context)).
      map(x => Map(PROP_VIEW_SERVICE -> ViewService(x, strategy))).
      getOrElse(Map.empty)

  private def _model_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] =
    parcel.getEffectiveModel.map(model_bindings(strategy, _)) getOrElse {
      Map(PROP_VIEW_MODEL -> ViewModel(EmptyModel, strategy))
    }

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

  protected def execute_Apply(engine: ViewEngine, parcel: Parcel): Content =
    StringContent(new UrlBag(url).toText, StaticPageExpires) // UTF-8
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

  protected def execute_Apply(engine: ViewEngine, parcel: Parcel): Content = {
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
    BinaryContent(mime, new UrlBag(url), StaticPageExpires)
  }
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

  protected def execute_Apply(engine: ViewEngine, parcel: Parcel): Content = {
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
    BinaryContent(mime, new UrlBag(url), AssetsExpires)
  }
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

trait TableViewBase extends ModelViewBase[ITableModel with Model] {
  // lazy val schema: Schema = model.schema getOrElse {
  //   strategy.withEntityType(model.getEntityType).resolveSchema
  // }
  // lazy val thead: THeadView = THeadView.create(this)
  // lazy val tbody: TBodyView = TBodyView.create(this)
}

case class TableView(model: ITableModel with Model) extends TableViewBase {
}

case class THeadView(model: TableHeadModel) extends ModelViewBase[TableHeadModel] {
}

case class TBodyView(model: TableBodyModel) extends ModelViewBase[TableBodyModel] {
}

case class TrView(model: IRecordModel with Model) extends ModelViewBase[IRecordModel with Model] {
}

case class ThView(model: TableHeadRecordDataModel) extends ModelViewBase[TableHeadRecordDataModel] {
}

case class TdView(model: TableBodyRecordDataModel) extends ModelViewBase[TableBodyRecordDataModel] {
}

// case class THeadView(elements: List[TrView], strategy: RenderStrategy) extends ModelViewBase[{
//   def foreach(p: TrView => Unit): Unit = elements.foreach(p)
// }
// object THeadView {
//   def create(p: ITableView): THeadView = {
//     val a = for (c <- p.schema.columns) yield {
//       ThView(c.label(p.strategy.locale), p.strategy)
//     }
//     val tr = TrView(a.toList, p.strategy)
//     THeadView(List(tr), p.strategy)
//   }
// }

// case class TBodyView(elements: List[TrView], strategy: RenderStrategy) {
//   def foreach(p: TrView => Unit): Unit = elements.foreach(p)
// }
// object TBodyView {
//   def create(p: ITableView): TBodyView = {
//     val a = for (x <- p.model.records) yield {
//       val b = for (c <- p.schema.columns) yield {
//         TdView(p.strategy.format(c, x), p.strategy)
//       }
//       TrView(b.toList, p.strategy)
//     }
//     TBodyView(a, p.strategy)
//   }
// }

// case class TrView(elements: List[TrViewElement], strategy: RenderStrategy) {
//   def foreach(p: TrViewElement => Unit): Unit = elements.foreach(p)
// }

// trait TrViewElement extends {
// }

// case class ThView(v: String, strategy: RenderStrategy) extends TrViewElement {
//   override def toString(): String = v
// }

// case class TdView(v: String, strategy: RenderStrategy) extends TrViewElement {
//   override def toString(): String = v
// }
