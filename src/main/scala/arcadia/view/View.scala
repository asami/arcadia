package arcadia.view

import scalaz._, Scalaz._
import scala.xml.NodeSeq
import java.io.File
import java.net.URL
import org.fusesource.scalate._
import org.goldenport.exception.RAISE
import org.goldenport.Strings
import org.goldenport.record.v2._
import org.goldenport.bag.{ChunkBag, UrlBag}
import org.goldenport.io.UrlUtils
import org.goldenport.io.IoUtils
import org.goldenport.trace.Result
import org.goldenport.value._
import org.goldenport.util.StringUtils
import org.goldenport.util.RegexUtils
import com.asamioffice.goldenport.io.UURL
import arcadia._
import arcadia.context._
import arcadia.model._
import ViewEngine.{PROP_VIEW_SERVICE, PROP_VIEW_MODEL, PROP_VIEW_FORM}
import ViewEngine.PROP_VIEW_PROPERTIES
import ViewEngine.PROP_VIEW_IT
import ViewEngine.Slot
import ViewEngine.Bindings

/*
 * @since   Jul. 15, 2017
 *  version Aug. 30, 2017
 *  version Sep. 30, 2017
 *  version Oct. 27, 2017
 *  version Nov. 14, 2017
 *  version Mar. 18, 2018
 *  version Jul. 21, 2019
 *  version Mar. 21, 2020
 *  version May. 28, 2020
 *  version Feb. 27, 2022
 *  version Mar. 28, 2022
 *  version Apr. 30, 2022
 *  version May. 22, 2022
 *  version Oct.  1, 2022
 *  version Apr. 30, 2023
 *  version Jun. 25, 2023
 * @version Mar. 20, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class View() {
  lazy val show: String = s"${getClass.getSimpleName}${show_info}"
  protected def show_info: String =
    if (Strings.blankp(show_Info))
      ""
    else
      s"($show_Info)"
  protected def show_Info: String = ""

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
    parcel.executeWithTrace(s"${show}#execute_apply", parcel.show) {
      val bindings = _build_bindings(engine, parcel)
      val c0 = execute_Apply(engine, parcel, bindings)
      val c = parcel.model.fold(c0) {
        case m: ErrorModel => c0.withCode(m.code)
        case _ => c0
      }
      val r = engine.eval(parcel, c, bindings)
      Result(r, r.show)
    }

  protected def execute_Apply(
    engine: ViewEngine,
    parcel: Parcel,
    bindings: Bindings
  ): Content

  private def _build_bindings(
    engine: ViewEngine,
    parcel: Parcel
  ): ViewEngine.Bindings = ViewEngine.Bindings(
    _build_bindings0(engine, parcel)
  )

  private def _build_bindings0(engine: ViewEngine, parcel: Parcel): Map[String, AnyRef] = {
    val strategy0 = parcel.render getOrElse PlainHtml
    val strategy = strategy0.withViewContext(engine, parcel)
    _model_bindings(strategy, parcel) ++
    _form_bindings(strategy, parcel) ++
    property_Bindings(strategy) ++
    _service_bindings(strategy, parcel) ++
    _properties_bindings(strategy, parcel)
//    _context_bindings(strategy, parcel)
  }

  private def _model_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] = {
    val a = parcel.getEffectiveModel.map(model_bindings(strategy, _)) getOrElse {
      Map(PROP_VIEW_MODEL -> ViewModel(EmptyModel, strategy))
    }
    a.get(PROP_VIEW_MODEL).fold(a) {
      case m: ViewModel => (a + (PROP_VIEW_IT -> m)) ++ m.bindings
      case m => a + (PROP_VIEW_IT -> m)
    }
  }

  protected def model_bindings(strategy: RenderStrategy, model: Model): Map[String, AnyRef] =
    model.viewBindings(strategy)

  private def _form_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] = {
    val x = parcel.getEffectiveModel.collect {
      case m: FormModel => ViewForm(m, strategy)
    }.getOrElse(ViewForm.undefined(strategy))
    Map(PROP_VIEW_FORM -> x)
  }

  protected def property_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty

  private def _service_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] =
    (parcel.context orElse strategy.viewContext.flatMap(_.parcel.context)).
      map(x => Map(PROP_VIEW_SERVICE -> ViewService(x, strategy, parcel.propertyModel))).
      getOrElse(Map.empty)

  private def _properties_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] =
    (parcel.context orElse strategy.viewContext.flatMap(_.parcel.context)).
      map(x => Map(PROP_VIEW_PROPERTIES -> ViewProperties(x, strategy))).
      getOrElse(Map.empty)
}

trait ModelViewBase[T <: Model] extends View {
  def model: T
  def guard: Guard = ModelNameGuard(model.featureName)
  protected def execute_Apply(
    engine: ViewEngine,
    parcel: Parcel,
    bindings: Bindings
  ): Content = {
    val p = parcel.forComponent(model)
    def s = _strategy(engine, p)
    engine.applyComponentOption(p) getOrElse model.apply(s)
  }

  private def _strategy(engine: ViewEngine, parcel: Parcel) = parcel.render.map(_.forComponent(engine, parcel)) getOrElse {
    RAISE.noReachDefect
  }
}

abstract class TemplateViewBase(template: TemplateSource) extends View() {
  def uri = template.uri
  def sourceName: String = StringUtils.pathLastComponentBody(uri)

  override def show_Info = StringUtils.shortUri(template.uri)

  protected def execute_Apply(
    engine: ViewEngine,
    parcel: Parcel,
    bindings: Bindings
  ): Content = {
//    val bindings = _build_bindings(engine, parcel)
    XmlContent(engine.render(template, parcel, bindings))
  }

//   private def _build_bindings(engine: ViewEngine, parcel: Parcel): Map[String, AnyRef] = {
//     val strategy0 = parcel.render getOrElse PlainHtml
//     val strategy = strategy0.withViewContext(engine, parcel)
//     _model_bindings(strategy, parcel) ++
//     _form_bindings(strategy, parcel) ++
//     property_Bindings(strategy) ++
//     _service_bindings(strategy, parcel) ++
//     _properties_bindings(strategy, parcel)
// //    _context_bindings(strategy, parcel)
//   }

//   private def _model_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] = {
//     val a = parcel.getEffectiveModel.map(model_bindings(strategy, _)) getOrElse {
//       Map(PROP_VIEW_MODEL -> ViewModel(EmptyModel, strategy))
//     }
//     a.get(PROP_VIEW_MODEL).
//       map(x => a + (PROP_VIEW_IT -> x)).
//       getOrElse(a)
//   }

//   protected def model_bindings(strategy: RenderStrategy, model: Model): Map[String, AnyRef] =
//     model.viewBindings(strategy)

//   private def _form_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] = {
//     val x = parcel.getEffectiveModel.collect {
//       case m: FormModel => ViewForm(m, strategy)
//     }.getOrElse(ViewForm.undefined(strategy))
//     Map(PROP_VIEW_FORM -> x)
//   }

//   protected def property_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty

//   private def _service_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] =
//     (parcel.context orElse strategy.viewContext.flatMap(_.parcel.context)).
//       map(x => Map(PROP_VIEW_SERVICE -> ViewService(x, strategy, parcel.propertyModel))).
//       getOrElse(Map.empty)

//   // Scalate uses variable context.
//   // private def _context_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] =
//   //   strategy.viewContext.
//   //     map(x => Map(PROP_VIEW_CONTEXT -> x)).
//   //     getOrElse(Map.empty)

//   private def _properties_bindings(strategy: RenderStrategy, parcel: Parcel): Map[String, AnyRef] =
//     (parcel.context orElse strategy.viewContext.flatMap(_.parcel.context)).
//       map(x => Map(PROP_VIEW_PROPERTIES -> ViewProperties(x, strategy))).
//       getOrElse(Map.empty)
}

case class TemplateView(
  guard: Guard,
  template: TemplateSource,
  properties: Map[String, AnyRef]
) extends TemplateViewBase(template) {
}

case class IndexView(template: TemplateSource) extends TemplateViewBase(template) {
  val guard = IndexGuard
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
object PageView {
  def create(p: File): PageView = {
    val name = StringUtils.pathLastComponentBody(p.getCanonicalPath)
    val ts = WebModule.toTemplateSource(p)
    PageView(name, ts)
  }
}

case class HtmlView(url: URL, pathname: Option[String] = None) extends View() {
  private val _pathname = pathname getOrElse UrlUtils.takeLeafName(url)
  val guard = PathnameGuard(_pathname)

  def sourceName: String = _pathname

  protected def execute_Apply(
    engine: ViewEngine,
    parcel: Parcel,
    bindings: Bindings
  ): Content =
    if (true)
      XmlContent.loadHtml(url)
    else
      StringContent(new UrlBag(url).toText, StaticPageExpires) // UTF-8
}
object HtmlView {
  def apply(p: File): HtmlView = HtmlView(p.toURI.toURL)
}

case class MaterialView(baseUrl: URL) extends View() {
  val guard = new Guard {
    def isAccept(p: Parcel): Boolean = p.command.fold(false) {
      case MaterialCommand(pathname) =>
        val url = new URL(baseUrl, pathname.v)
        UrlUtils.isExist(url)
      case _ => false
    }
  }

  protected def execute_Apply(
    engine: ViewEngine,
    parcel: Parcel,
    bindings: Bindings
  ): Content = {
    val c = parcel.takeCommand[MaterialCommand]
    val mime = {
      val a = for {
        suffix <- c.pathname.getSuffix
        context <- parcel.context
        mime <- context.getMimetypeBySuffix(suffix)
      } yield mime
      a.getOrElse(MimeType.application_octet_stream)
    }
    _get_control_content(parcel, c.pathname.v) getOrElse {
      val url = new URL(baseUrl, c.pathname.v)
      def loadedstring = parcel.context.map(_.loadString(url)).getOrElse(IoUtils.toText(url))
      if (mime.isXml)
        XmlContent.load(mime, url)
      else if (mime.isHtml)
        StringContent(mime, loadedstring)
      else if (mime.isText)
        StringContent(mime, loadedstring)
      else
        BinaryContent(mime, new UrlBag(url), AssetsExpires)
    }
  }

  def getControlContent(parcel: Parcel): Option[Content] = {
    val c = parcel.takeCommand[MaterialCommand]
    _get_control_content(parcel, c.pathname.v)
  }

  private def _get_control_content(
    parcel: Parcel,
    pathname: String
  ): Option[Content] = {
    val url = new URL(baseUrl, pathname)
    if (UrlUtils.isExist(url)) {
      if (StringUtils.getSuffix(pathname).isEmpty)
        Some(RedirectContent(_redirect_pathname(parcel, pathname, "index.html")))
      else
        None
    } else {
      Some(NotFoundContent(pathname))
    }
  }

  private def _redirect_pathname(
    parcel: Parcel,
    pathname: String,
    filename: String
  ) =
    parcel.context.flatMap(_.platformExecutionContext.getImplicitIndexBase).
      map(base =>
        StringUtils.concatPath(StringUtils.concatPath(base, pathname), filename)
      ).getOrElse(StringUtils.concatPath(pathname, filename))

  // private def _is_redirect(pathname: String): Boolean =
  //   StringUtils.getSuffix(pathname).isEmpty
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

  protected def execute_Apply(
    engine: ViewEngine,
    parcel: Parcel,
    bindings: Bindings
  ): Content = {
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

case class EntityScenarioView(
  slots: Vector[EntityScenarioView.Slot]
) extends View with Guard {
  import EntityScenarioView.Slot.{Scenario => SScenario}
  import EntityScenarioView.Slot.{Action => SAction}

  def guard: arcadia.Guard = this

  def isAccept(p: Parcel) = {
    val a = for {
      name <- _get_scenario_name(p.getOperationName)
      model <- p.model
    } yield _is_accept(name, model)
    a getOrElse false
  }

  private def _get_scenario_name(p: Option[String]) = for {
    name <- p
    body = StringUtils.pathLastComponentBody(name)
    r <- RegexUtils.getString(EntityScenarioView.ScenarioRegex, body, 1)
  } yield r

  private def _is_accept(name: String, model: Model): Boolean = {
    val a = for {
      scenario <- _scenario(name, model)
      action <- _action(name, model)
    } yield _is_accept(scenario, action)
    a getOrElse false
  }

  private def _is_accept(s: SScenario, a: SAction): Boolean =
    slots.exists(_.isAccept(s, a))

  private def _find_view(name: String, model: Model): Option[View] = for {
    scenario <- _scenario(name, model)
    action <- _action(name, model)
    r <- _find_view(scenario, action)
  } yield r

  private def _find_view(s: SScenario, a: SAction): Option[View] =
    slots.find(_.isAccept(s, a)).map(_.view)

  private def _scenario(name: String, model: Model): Option[SScenario] =
    SScenario.get(name)

  private def _action(name: String, model: Model): Option[SAction] =
    Option(model).collect {
      case m: PropertyInputFormModel => SAction.Input
      case m: PropertyConfirmFormModel => SAction.Confirm
      case m: PropertyShowFormModel => SAction.Show
    }

  protected def execute_Apply(
    engine: ViewEngine,
    p: Parcel,
    bindings: Bindings
  ): Content = {
    val a = for {
      name <- _get_scenario_name(p.getOperationName)
      model <- p.model
      view <- _find_view(name, model)
    } yield view.apply(engine, p)
    a getOrElse RAISE.noReachDefect
  }
}
object EntityScenarioView {
  import EntityScenarioView.Slot.{Scenario => SScenario}
  import EntityScenarioView.Slot.{Action => SAction}

  val ScenarioRegex = """_([^_]+)_""".r

  case class Slot(
    scenario: Slot.Scenario,
    action: Slot.Action,
    view: View
  ) {
    def isAccept(s: SScenario, a: SAction) = s == scenario && a == action
  }
  object Slot {
    sealed trait Scenario extends NamedValueInstance {
    }
    object Scenario extends EnumerationClass[Scenario] {
      val elements = Vector(Create, Update, Delete)

      case object Create extends Scenario {
        val name = "create"
      }
      case object Update extends Scenario {
        val name = "update"
      }
      case object Delete extends Scenario {
        val name = "delete"
      }
    }

    sealed trait Action extends NamedValueInstance {
    }
    object Action extends EnumerationClass[Action] {
      val elements = Vector(Input, Confirm, Show)

      case object Input extends Action {
        val name = "input"
      }
      case object Confirm extends Action {
        val name = "confirm"
      }
      case object Show extends Action {
        val name = "show"
      }
    }

    def createOption(p: View): Option[Slot] = {
      val name: Option[String] = p match {
        case m: TemplateView => Some(m.sourceName)
        case m: HtmlView => Some(m.sourceName)
        case _ => None
      }
      for {
        n <- name
        x <- StringUtils.getNameDirective(n)
        (s, directive) = x
        scenario <- Scenario.get(s)
        action <- Action.get(directive)
      } yield Slot(scenario, action, p)
    }
  }

  def create(ps: Seq[View]): EntityScenarioView = {
    val a = ps.flatMap(Slot.createOption)
    EntityScenarioView(a.toVector)
  }
}
