package arcadia.view

import scala.collection.mutable
import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import java.util.{Locale, Date}
import java.net.URI
import java.sql.Timestamp
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.goldenport.Strings.blankopt
import org.goldenport.exception.RAISE
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => _, _}
import org.goldenport.record.v2.util.{SchemaBuilder, RecordUtils}
import org.goldenport.i18n.I18NElement
import org.goldenport.xml.XhtmlUtils
import org.goldenport.value._
import org.goldenport.values.PathName
import org.goldenport.util.{MapUtils, StringUtils, AnyUtils}
import arcadia._
import arcadia.context._
import arcadia.model.Picture
import arcadia.domain._
import arcadia.view.ViewEngine._

/*
 * @since   Jul. 31, 2017
 *  version Aug. 29, 2017
 *  version Sep. 27, 2017
 *  version Oct. 30, 2017
 *  version Nov. 22, 2017
 *  version Dec. 30, 2017
 *  version Jan. 15, 2018
 *  version Feb. 17, 2018
 *  version Mar. 21, 2018
 *  version May.  6, 2018
 *  version Aug.  5, 2018
 *  version Sep.  1, 2018
 *  version Nov.  7, 2018
 *  version Aug.  5, 2019
 *  version Mar. 31, 2020
 *  version Apr.  1, 2020
 *  version Apr. 30, 2022
 *  version May.  3, 2022
 *  version Dec. 30, 2022
 *  version Nov. 28, 2023
 *  version Dec. 28, 2023
 * @version Apr.  2, 2025
 * @author  ASAMI, Tomoharu
 */
case class RenderStrategy(
  locale: Locale,
  theme: RenderTheme,
  schema: SchemaRule,
  applicationRule: WebApplicationRule,
  partials: Partials,
  components: Components,
  layoutKind: Option[LayoutKind],
  renderContext: RenderContext,
  viewContext: Option[ViewContext],
  history: RenderStrategy.History = RenderStrategy.History.empty // unused
) {
  def scope = renderContext.scope
  def size = renderContext.size getOrElse NormalSize
  def tableKind = (
    renderContext.tableKind orElse
      history.tableKind orElse // unused
      applicationRule.renderStrategy.tableKind
      getOrElse theme.default.tableKind
  )
  def tableKind(p: Option[TableKind]) = (
    renderContext.tableKind orElse
      history.tableKind orElse // unused
      applicationRule.renderStrategy.tableKind orElse
      p getOrElse theme.default.tableKind
  )
  def cardKind = (
    renderContext.cardKind orElse
      history.cardKind orElse // unused
      applicationRule.renderStrategy.cardKind getOrElse theme.default.cardKind
  )
  def cardKind(p: Option[TableKind]) = (
    renderContext.cardKind orElse
      history.cardKind orElse
      applicationRule.renderStrategy.cardKind orElse
      p getOrElse theme.default.cardKind
  )
  def cardKindInGrid = renderContext.cardKindInGrid orElse applicationRule.renderStrategy.cardKindInGrid getOrElse theme.default.cardKindInGrid
  def isLogined = executeOption(_.isLogined) getOrElse false
  def getOperationName: Option[String] = executeOption(_.getOperationName).flatten
  def gridContext: GridContext = renderContext.gridContext getOrElse theme.default.gridContext(this)
  lazy val noImageIcon: Picture = Picture.create(theme.default.noImageIcon)
  lazy val noImagePicture: Picture = Picture.create(theme.default.noImagePicture)
//  lazy val formatter = renderContext.formatter.withLocale(locale)

  def formatter = renderContext.formatter
  lazy val getWebApplicationRule: Option[WebApplicationRule] = viewContext.flatMap(_.parcel.render.map(_.applicationRule))
  lazy val getView: Option[View] = viewContext.flatMap(_.parcel.view)
  lazy val getPage: Option[WebApplicationRule.Page] = for {
    rule <- getWebApplicationRule
    view <- getView
    page <- rule.getPage(view)
  } yield page

  def show: String = s"RenderStrategy"

  def withScopeHtml = if (scope == Html) this else copy(renderContext = renderContext.withScopeHtml)
  def withScopeSection = if (scope == Section) this else copy(renderContext = renderContext.withScopeSection)
  def withScopeSectionDown = scope match {
    case Html => copy(renderContext = renderContext.withScopeSection)
    case Body => copy(renderContext = renderContext.withScopeSection)
    case Section => copy(renderContext = renderContext.sectionDown)
    case Content => this
  }
  def withScopeContent = if (scope == Content) this else copy(renderContext = renderContext.withScopeContent)

  def withSizeTiny = if (size == TinySize) this else copy(renderContext = renderContext.withSizeTiny)

  def withGridContext(p: GridContext) = copy(renderContext = renderContext.withGridContext(p))

  def withEntityType(p: Option[DomainEntityType]) = copy(renderContext = renderContext.withEntityType(p))
  def withEntityType(p: DomainEntityType) = copy(renderContext = renderContext.withEntityType(p))
  def withUsageKind(p: UsageKind) = copy(renderContext = renderContext.withUsageKind(p))
  def withTableKind(p: TableKind) = copy(renderContext = renderContext.withTableKind(p))
//  private def withCardKind(p: CardKind) = copy(renderContext = renderContext.withCardKind(p))
  def withCardKindInGrid(p: CardKind) = copy(renderContext = renderContext.withCardKindInGrid(p))
  def withFormatter(p: FormatterContext) = copy(renderContext = renderContext.withFormatter(p))

  def withViewContext(engine: ViewEngine, parcel: Parcel) = copy(viewContext = Some(ViewContext(engine, parcel)))
  def withThemeComponentsPartials(t: RenderTheme, cs: Components, p: Partials) = copy(
    theme = t,
    components = cs,
    partials = p
  )
  def complementApplicationRule(p: WebApplicationRule) = copy(applicationRule = applicationRule.complement(p))

  def withLayoutKind(p: LayoutKind) = copy(layoutKind = Some(p))

  def push(p: RenderStrategy) = copy(history = history.append(p))

  def forComponent(engine: ViewEngine, parcel: Parcel) = forView(engine, parcel)
  def forView(engine: ViewEngine, parcel: Parcel) =
    if (viewContext.fold(false)(_.isMatch(engine, parcel)))
      this
    else
      copy(viewContext = Some(ViewContext(engine, parcel)))

  def addJavaScriptInFooter(p: String): Unit = renderContext.addJavaScriptInFooter(p)

  def getConfigString(p: String): Option[String] = applicationRule.getString(p)

  def getEntityType = renderContext.entityType

  def execute[T](pf: ExecutionContext => T): T =
    executeOption(pf) getOrElse RAISE.noReachDefect

  def executeOption[T](pf: ExecutionContext => T): Option[T] =
    viewContext.flatMap(_.parcel.executeOption(pf))

  def resolveSchema(p: Renderer.TableOrder): Schema = schema.resolve(this, p)

  def resolveSchema(entitytype: DomainEntityType, s: Schema): Schema = schema.resolve(this, entitytype, s)

  def format(column: Column, rec: IRecord): String = {
    rec.get(column.name).map {
      case m => column.datatype.format(m)
    }.getOrElse("")
  }

  def format(p: Any): String = p match {
    case m: Timestamp => formatDateTime(m)
    case m: Date => formatDate(m)
    case _ => AnyUtils.toString(p)
  }

  def formatDateTime(p: Any): String =
    formatter.formatDateTime(p)
//    formatter.datetime.print(AnyUtils.toDateTime(p))

  def formatDate(p: Any): String =
    formatter.formatDate(p)
//    formatter.date.print(AnyUtils.toLocalDate(p))

  def formatTime(p: Any): String =
    formatter.formatTime(p)
//    formatter.time.print(AnyUtils.toLocalTime(p))

  def formatXml(p: Any): NodeSeq =
    XhtmlUtils.parseNode(p.toString)

  def generateId(): String = renderContext.generateId()

  object label {
    def commandSubmit: I18NElement = renderContext.label.commandSubmit
    def buttonSearch: I18NElement = renderContext.label.buttonSearch
    def placeholderStart: I18NElement = renderContext.label.placeholderStart
    def placeholderEnd: I18NElement = renderContext.label.placeholderEnd
  }
}
object RenderStrategy {
  // Unused
  case class History(
    history: Vector[RenderStrategy] = Vector.empty
  ) {
    def append(p: RenderStrategy) = copy(history = history :+ p)

    def tableKind: Option[TableKind] = history.toStream.flatMap(_.applicationRule.renderStrategy.tableKind).headOption

    def cardKind: Option[CardKind] = history.toStream.flatMap(_.applicationRule.renderStrategy.cardKind).headOption
  }
  object History {
    val empty = History()
  }
}

sealed trait RenderScope {
}
case object Html extends RenderScope
case object Body extends RenderScope
case object Section extends RenderScope
case object Content extends RenderScope

sealed trait RenderSize {
  def cssClass: String
}
case object NormalSize extends RenderSize { // 14pt
  val cssClass = "arcadia-normal-size"
}
case object SmallSize extends RenderSize { // 12pt
  val cssClass = "arcadia-small-size"
}
case object VerySmallSize extends RenderSize { // 10pt
  val cssClass = "arcadia-very-small-size"
}
case object TinySize extends RenderSize { // 8pt
  val cssClass = "arcadia-tiny-size"
}

case class GridContext(
  width: Int,
  ncolumns: Map[ScreenKind, Int], // xs, sm, md, lg, lx
  isNoGutters: Boolean = false
) {
  def withTabletColumns(ncolumns: Int): GridContext = {
    val a: Map[ScreenKind, Int] = ncolumns match {
      case 0 => GridContext.banner.ncolumns
      case 1 => Map(
        TabletScreen -> 1,
        PhabletScreen -> 1,
        PhoneScreen -> 1
      )
      case 2 => Map(
        TabletScreen -> 2,
        PhabletScreen -> 1,
        PhoneScreen -> 1
      )
      case 3 => Map(
        TabletScreen -> 3,
        PhabletScreen -> 1,
        PhoneScreen -> 1
      )
      case 4 => Map(
        TabletScreen -> 4,
        PhabletScreen -> 2,
        PhoneScreen -> 1
      )
      case _ => GridContext.banner.ncolumns
    }
    copy(ncolumns = a)
  }
  def defaultNColumns: Int = 4 // TODO customizable
}
object GridContext {
  val image = GridContext(
    12,
    Map(
      DesktopScreen -> 12,
      LaptopScreen -> 12,
      TabletScreen -> 6,
      PhabletScreen -> 4,
      PhoneScreen -> 3
    )
  )
  val card = GridContext(
    12,
    Map(
      DesktopScreen -> 12,
      LaptopScreen -> 6,
      TabletScreen -> 4,
      PhabletScreen -> 2,
      PhoneScreen -> 1
    )
  )
  val banner = GridContext(
    12,
    Map(
      TabletScreen -> 1,
      PhabletScreen -> 1,
      PhoneScreen -> 1
    ),
    true
  )
}

sealed trait RenderTheme extends ClassNamedValueInstance {
  protected def name_Suffix = "Theme"
  def isGridTable: Boolean = false
  def isGridDiv: Boolean = false
  def isCardTable: Boolean = false
  def isCardDiv: Boolean = false
  object default {
    def noImageIcon: URI = new URI(default_No_Image_Icon)
    def noImagePicture: URI = new URI(default_No_Image_Picture)
    def noUserImageIcon: URI = new URI(default_No_User_Image_Icon)
    def usageKind: UsageKind = default_UsageKind
    def tableKind: TableKind = default_TableKind
    def cardKind: CardKind = default_CardKind
    def cardKindInGrid: CardKind = default_CardKind_In_Grid
    def cardKindInList: CardKind = default_CardKind_In_List
    def gridContext(strategy: RenderStrategy): GridContext = default_GridContext(strategy)
  }
  object head {
    def charset(strategy: RenderStrategy): Node = <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    def keywords(strategy: RenderStrategy): Node = <meta name="keywords" content="TBD"/>
    def description(strategy: RenderStrategy): Node = <meta name="description" content="TBD"/>
    def robots(strategy: RenderStrategy): Node = <meta name="robots" content="noindex,nofollow"/>
    def author(strategy: RenderStrategy): Node = <meta name="author" content="TBD"/>
    def theme(strategy: RenderStrategy): Node = Group(Nil)
  }
  object body {
    def cssClass(strategy: RenderStrategy): String = body_CssClass(strategy)
  }
  object table {
    def container(p: Renderer.Table, body: => Node): Node = table_Container(p, body)
    object css {
      def caption(p: Renderer.Table) = table_CssClass_Caption(p)
      def table(p: Renderer.Table) = table_CssClass_Table(p)
      def thead(p: Renderer.Table) = table_CssClass_Thead(p)
      def tbody(p: Renderer.Table) = table_CssClass_Tbody(p)
      def tfoot(p: Renderer.Table) = table_CssClass_Tfoot(p)
      def theadTr(p: Renderer.Table) = table_CssClass_TheadTr(p)
      def theadTh(p: Renderer.TableColumn) = table_CssClass_TheadTh(p)
      def tbodyTr(p: Renderer.Table) = table_CssClass_TbodyTr(p)
      def getTbodyTr(p: Renderer.Table) = blankopt(table_CssClass_TbodyTr(p))
      def tbodyTd(p: Renderer.TableColumn) = table_CssClass_TbodyTd(p)
      def tfootTr(p: Renderer.Table) = table_CssClass_TfootTr(p)
      def tfootTd(p: Renderer.TableColumn) = table_CssClass_TfootTd(p)
      def img(p: TableKind) = table_CssClass_Img(p)
    }
  }
  object grid {
    object css {
      object table {
        def container = grid_CssClass_Table_Container
        def table = grid_CssClass_Table_Table
        def tbody = grid_CssClass_Table_Tbody
        def tbodyTr = grid_CssClass_Table_TbodyTr
      }
      object div {
        def container = grid_CssClass_Div_Container
        def row = grid_CssClass_Div_Row
        def field = grid_CssClass_Div_Field
      }
    }
  }
  object card {
    object css {
      object table {
        def td = card_CssClass_Table_Td
        def anchor = card_CssClass_Table_Anchor
      }
      object div {
        def anchor = card_CssClass_Div_Anchor
        def container = card_CssClass_Div_Container
        def card = card_CssClass_Div_Card
        def imageTop = card_CssClass_Div_ImageTop
        def header = card_CssClass_Div_Header
        def footer = card_CssClass_Div_Footer
        def content = card_CssClass_Div_Content
      }
    }
  }
  object sidebar {
    // def content(view: ViewModel): Node = Group(
    //   <li class="active">
    //     <a href="XXX">
    //     <i class="ti-panel">&#8203;</i>
    //     <p>Dashboard</p>
    //     </a>
    //   </li>
    //   <li>
    //     <a href="XXX">
    //     <i class="ti-user">&#8203;</i>
    //     <p>User Profile</p>
    //     </a>
    //   </li>
    // )
    def content(view: ViewModel): Node = {
      def features(p: WebApplicationRule.FeatureList) = {
        val xs = p.page.map(sidebar_Feature_Item(view, _))
        Group(xs)
      }
      view.strategy.applicationRule.feature_list.
        map(features).
        getOrElse(Group(Nil))
    }
  }
  object navigation {
    def content(view: ViewModel): Node = navigation_Content(view)
  }

  protected def default_UsageKind: UsageKind = ListUsage
  protected def default_TableKind: TableKind = StandardTable
  protected def default_CardKind: CardKind = FullCard
  protected def default_CardKind_In_Grid: CardKind = ImageTitleCard
  protected def default_CardKind_In_List: CardKind = MediaObjectCard
  protected def default_GridContext(strategy: RenderStrategy): GridContext = GridContext.card
  protected def default_No_Image_Icon: String = "assets/img/no-image-icon.png"
  protected def default_No_Image_Picture: String = "assets/img/no-image-picture.png"
  protected def default_No_User_Image_Icon: String = "assets/img/no-user-image-icon.png"
  protected def body_CssClass(strategy: RenderStrategy): String = ""
  protected def table_Container(p: Renderer.Table, body: => Node): Node = body
  protected def table_CssClass_Caption(p: Renderer.Table): String = ""
  protected def table_CssClass_Table(p: Renderer.Table): String = ""
  protected def table_CssClass_Thead(p: Renderer.Table): String = ""
  protected def table_CssClass_Tbody(p: Renderer.Table): String = ""
  protected def table_CssClass_Tfoot(p: Renderer.Table): String = ""
  protected def table_CssClass_TheadTr(p: Renderer.Table): String = ""
  protected def table_CssClass_TheadTh(p: Renderer.TableColumn): String = ""
  protected def table_CssClass_TbodyTr(p: Renderer.Table): String = ""
  protected def table_CssClass_TbodyTd(p: Renderer.TableColumn): String = ""
  protected def table_CssClass_TfootTr(p: Renderer.Table): String = ""
  protected def table_CssClass_TfootTd(p: Renderer.TableColumn): String = ""
  protected def table_CssClass_Img(p: TableKind): String = ""

  protected def grid_CssClass_Table_Container: String = ""
  protected def grid_CssClass_Table_Table: String = ""
  protected def grid_CssClass_Table_Tbody: String = ""
  protected def grid_CssClass_Table_TbodyTr: String = ""
  protected def grid_CssClass_Div_Container: String = ""
  protected def grid_CssClass_Div_Row: String = ""
  protected def grid_CssClass_Div_Field: String = ""

  protected def card_CssClass_Table_Td: String = ""
  protected def card_CssClass_Table_Anchor: String = ""
  protected def card_CssClass_Div_Anchor: String = ""
  protected def card_CssClass_Div_Container: String = ""
  protected def card_CssClass_Div_Card: String = ""
  protected def card_CssClass_Div_ImageTop: String = ""
  protected def card_CssClass_Div_Header: String = ""
  protected def card_CssClass_Div_Footer: String = ""
  protected def card_CssClass_Div_Content: String = ""

  protected def sidebar_Feature_Item(
    view: ViewModel,
    p: WebApplicationRule.Page
  ): Node = {
    val isactive = view.isActiveFeature(p.name)
    val icon = IconFactory.guessTiIcon(p.name)
    val href = if (isactive) "#" else s"${p.name}.html"
    def anchor = <a href={href}>
      <i class={icon}>&#8203;</i>
      <p>{p.title(view.locale)}</p>
    </a>
    if (isactive)
      <li class="active">{anchor}</li>
    else
      <li>{anchor}</li>
  }

  protected def navigation_Content(view: ViewModel): Node = Group(
    <li>
      <a href="#" class="dropdown-toggle" data-toggle="dropdown">
      <i class="ti-panel">&#8203;</i>
      <p>Stats</p>
      </a>
    </li>
  )
}
object RenderTheme extends EnumerationClass[RenderTheme] {
  val elements = Vector(
    PlainTheme,
    PaperDashboardTheme,
    MatrialKitTheme,
    NowUiKitTheme,
    NowUiDashboardProTheme,
    MyColorTheme,
    LightBootstrapDashboardTheme,
    BootstrapTheme, // Latest Bootstrap 5
    BootstrapListTheme,
    BootstrapGridTheme,
    MaterialKitProTheme
  )
}

sealed trait BootstrapRenderThemeBase extends RenderTheme {
  override def isGridDiv = true
  override def isCardDiv = true
  // protected def default_CardKind_In_Grid: CardKind = ComponentCard

  override protected def table_Container(p: Renderer.Table, body: => Node): Node = p.kind match {
    case StandardTable => _table_container_standard(body)
    case ListTable => _table_container_list(body)
    case GridTable => _table_container_grid(body)
    case TabularTable => _table_container_card(body) // TODO
    case PropertyTable => _table_container_standard(body) // TODO
    case EntityTable => _table_container_standard(body) // TODO
    case FormTable => _table_container_standard(body) // TODO
    case DashboardTable => _table_container_dashboard(body)
  }

  private def _table_container_standard(body: => Node): Node = // TODO
    <div class="table-responsive">
      {body}
    </div>

  private def _table_container_list(body: => Node): Node = {body}

  private def _table_container_grid(body: => Node): Node = {body}

  private def _table_container_card(body: => Node): Node = 
    <div class="table-responsive">
      {body}
    </div>

  private def _table_container_dashboard(body: => Node): Node = 
    <div class="table-responsive">
      {body}
    </div>

  override protected def table_CssClass_Table(p: Renderer.Table): String =
    p.kind match {
      case StandardTable => "table"
      case ListTable => "table table-hover table-responsive"
      case GridTable => "table table-hover table-responsive"
      case TabularTable => ""
      case PropertyTable => "table table-bordered table-sm"
      case EntityTable => "table table-bordered table-sm"
      case FormTable => ""
      case DashboardTable => "table table-hover table-responsive table-sm"
    }
  // table, table-striped, table-bordered, table-hover, table-sm, table-responsive

  // tr/td: active, success, info, warning, danger

  override protected def navigation_Content(view: ViewModel): Node = Group(
    <li>
      <a href="#" class="dropdown-toggle" data-toggle="dropdown">
      <i class="ti-panel">&#8203;</i>
      <p>Stats</p>
      </a>
    </li>
  )
}

trait Bootstrap3RenderThemeBase extends BootstrapRenderThemeBase {
}

trait Bootstrap4RenderThemeBase extends BootstrapRenderThemeBase {
}

trait Bootstrap5RenderThemeBase extends BootstrapRenderThemeBase {
}

case object PlainTheme extends RenderTheme {
}
case object PaperDashboardTheme extends Bootstrap3RenderThemeBase {
  override protected def default_TableKind: TableKind = StandardTable
  override protected def table_CssClass_TheadTh(p: Renderer.TableColumn): String =
    p.size.cssClass
}
case object MatrialKitTheme extends Bootstrap3RenderThemeBase {
}
case object NowUiKitTheme extends Bootstrap4RenderThemeBase {
  override def body_CssClass(strategy: RenderStrategy): String = {
    def default = "landing-page sidebar-collapse"
    strategy.getOperationName.map {
      case "index" => "index-page sidebar-collapse"
      case "login" => "login-page sidebar-collapse"
      case "logout" => "login-page sidebar-collapse"
      case _ => default
    }.getOrElse(default)
  }
}

case object MyColorTheme extends Bootstrap4RenderThemeBase {
  override val isCardDiv = true

  override protected def default_GridContext(strategy: RenderStrategy) =
    GridContext(
      12,
      Map(
        DesktopScreen -> 12,
        LaptopScreen -> 12,
        TabletScreen -> 6,
        PhabletScreen -> 4,
        PhoneScreen -> 3
      )
    )

  override protected def default_No_Image_Icon = "assets/images/sample-good.png"
  override protected def grid_CssClass_Div_Container: String = "c-cardList"
  override protected def grid_CssClass_Div_Row: String = ""
  override protected def grid_CssClass_Div_Field: String = ""
  override protected def card_CssClass_Div_Container: String = ""
  override protected def card_CssClass_Div_Anchor: String = "c-card"
  override protected def card_CssClass_Div_Card: String = ""
  override protected def card_CssClass_Div_ImageTop: String = "c-card__imageArea"
  override protected def card_CssClass_Div_Content: String = ""
}

case object LightBootstrapDashboardTheme extends Bootstrap4RenderThemeBase {
  override protected def sidebar_Feature_Item(
    view: ViewModel,
    p: WebApplicationRule.Page
  ): Node = {
    val isactive = view.isActiveFeature(p.name)
    val icon = IconFactory.guessNcIcon(p.name)
    val pathname = view.resolvePathName(p.name)
    val href = if (isactive) "#" else s"${pathname.v}.html"
    def anchor = <a class="nav-link" href={href}>
      <i class={s"nc-icon ${icon}"}>&#8203;</i>
      <p>{p.title(view.locale)}</p>
    </a>
    if (isactive)
      <li class="nav-item active">{anchor}</li>
    else
      <li>{anchor}</li>
  }

  override protected def navigation_Content(view: ViewModel): Node = Group(
    List(
      <ul class="nav navbar-nav mr-auto">
        <li class="nav-item">
          <a class="nav-link" href="#" data-toggle="dropdown">
            <i class="nc-icon nc-palette"></i>
            <span class="d-lg-none">Stats</span>
          </a>
        </li>
      </ul>,
      <ul class="nav navbar-nav ml-auto">{
        List(
          _nav_login_logout(view)
        )
      }</ul>
    )
  )

  private def _nav_login_logout(view: ViewModel): Node =
    if (view.isLogined)
      _nav_logout
    else
      _nav_login


  private def _nav_login: Node = {
    <li class="nav-item">
      <a class="nav-link" href="login.html">
        <span class="no-icon">Login</span>
      </a>
    </li>
  }

  private def _nav_logout: Node = {
    <li class="nav-item">
      <a class="nav-link" href="logout.html">
        <span class="no-icon">Logout</span>
      </a>
    </li>
  }
}

case object NowUiDashboardProTheme extends Bootstrap4RenderThemeBase {
  override def body_CssClass(strategy: RenderStrategy): String = {
    def default = "sidebar-mini"
    strategy.getOperationName.map {
      case "index" => default
      case "login" => default
      case "logout" => default
      case _ => default
    }.getOrElse(default)
  }

  override protected def sidebar_Feature_Item(
    view: ViewModel,
    p: WebApplicationRule.Page
  ): Node = {
    val isactive = view.isActiveFeature(p.name)
    val icon = IconFactory.guessNowUiIcon(p.icon getOrElse p.name)
    val pathname = view.resolvePathName(p.pathname)
    val href = if (isactive) "#" else s"${pathname.v}.html"
    def anchor = <a href={href}>
      <i class={s"now-ui-icons ${icon}"}></i>
      <p>{p.title(view.locale)}</p>
    </a>
    if (isactive)
      <li class="active">{anchor}</li>
    else
      <li>{anchor}</li>
  }

  override protected def navigation_Content(view: ViewModel): Node = Group(
    List(
      <ul class="navbar-nav">
        <li class="nav-item">
          <a class="nav-link" href="#" data-toggle="dropdown">
            <i class="now-ui-icons media-2_sound-wave"></i>
            <p>
              <span class="d-lg-none d-md-block">Stats</span>
            </p>
          </a>
        </li>
      </ul>,
      <ul class="navbar-nav">{
        List(
          _nav_login_logout(view)
        )
      }</ul>
    )
  )

  private def _nav_login_logout(view: ViewModel): Node =
    if (view.isLogined)
      _nav_logout
    else
      _nav_login


  private def _nav_login: Node = {
    <li class="nav-item">
      <a class="nav-link" href="login.html">
        <span class="no-icon">Login</span>
      </a>
    </li>
  }

  private def _nav_logout: Node = {
    <li class="nav-item">
      <a class="nav-link" href="logout.html">
        <span class="no-icon">Logout</span>
      </a>
    </li>
  }
}

case object BootstrapTheme extends Bootstrap5RenderThemeBase {
}

case object BootstrapListTheme extends Bootstrap5RenderThemeBase {
  override def default_TableKind = ListTable
}

case object BootstrapGridTheme extends Bootstrap5RenderThemeBase {
  override def default_TableKind = GridTable
}

case object MaterialKitProTheme extends Bootstrap5RenderThemeBase {
  override def default_TableKind = GridTable
}

sealed trait TableKind extends NamedValueInstance {
}
object TableKind extends EnumerationClass[TableKind] {
  val elements = Vector(
    StandardTable,
    ListTable,
    GridTable,
    TabularTable,
    PropertyTable,
    EntityTable,
    FormTable,
    DashboardTable
  )
}
case object StandardTable extends TableKind {
  val name = "standard"
}
case object ListTable extends TableKind {
  val name = "list"
}
case object GridTable extends TableKind {
  val name = "grid"
}
case object TabularTable extends TableKind {
  val name = "tabular"
}
case object PropertyTable extends TableKind {
  val name = "property"
}
case object EntityTable extends TableKind {
  val name = "entity"
}
case object FormTable extends TableKind {
  val name = "form"
}
case object DashboardTable extends TableKind {
  val name = "dashboard"
}

sealed trait CardKind extends NamedValueInstance {
  def isImageTop: Boolean
  def isHeader: Boolean
  def isFooter: Boolean
  def isContent: Boolean
}
object CardKind extends EnumerationClass[CardKind] {
  val elements = Vector(
    ImageCard,
    ImageTitleCard,
    ImageTitleSummaryCard,
    ImageTitleContentCard
  )
  def take(p: String) = get(p) getOrElse ComponentCard(p)
}
case object FullCard extends CardKind {
  val name = "full"
  def isImageTop: Boolean = true
  def isHeader: Boolean = true
  def isFooter: Boolean = true
  def isContent: Boolean = true
}
case object ImageCard extends CardKind {
  val name = "image"
  def isImageTop: Boolean = true
  def isHeader: Boolean = false
  def isFooter: Boolean = false
  def isContent: Boolean = false
}
case object ImageTitleCard extends CardKind {
  val name = "image_title"
  def isImageTop: Boolean = true
  def isHeader: Boolean = true
  def isFooter: Boolean = false
  def isContent: Boolean = false
}
case object ImageTitleSummaryCard extends CardKind {
  val name = "image_title_summary"
  def isImageTop: Boolean = true
  def isHeader: Boolean = true
  def isFooter: Boolean = false
  def isContent: Boolean = true
}
case object ImageTitleContentCard extends CardKind {
  val name = "image_title_content"
  def isImageTop: Boolean = true
  def isHeader: Boolean = true
  def isFooter: Boolean = false
  def isContent: Boolean = true
}
case object MediaObjectCard extends CardKind {
  val name = "mediaobject"
  def isImageTop: Boolean = true
  def isHeader: Boolean = true
  def isFooter: Boolean = false
  def isContent: Boolean = true
}
case class ComponentCard(name: String) extends CardKind {
  def isImageTop: Boolean = true
  def isHeader: Boolean = true
  def isFooter: Boolean = true
  def isContent: Boolean = true
}
case class BrokenCard(m: String) extends CardKind {
  val name = "broken"
  def isImageTop: Boolean = false
  def isHeader: Boolean = false
  def isFooter: Boolean = false
  def isContent: Boolean = false
}
// object CardKind extends EnumerationClass[CardKind] {
//   val elements = Vector(
//     BootstrapCard, // Bootstrap4
//     PaperDashboardCard // Creative-Tim PaperDashboard
//   )
// }
// case object BootstrapCard extends CardKind {
//   val name = "bootstrap"
// }
// case object PaperDashboardCard extends CardKind {
//   val name = "paper-dashboard"
// }

sealed trait OperationMode {
}
case object MediaOperationMode extends OperationMode
case object ConsoleOperationMode extends OperationMode

sealed trait ScreenKind {
}
case object DesktopScreen extends ScreenKind // xl: >= 1200px
case object LaptopScreen extends ScreenKind // lg: >= 992px
case object TabletScreen extends ScreenKind // md: >= 768px
case object PhabletScreen extends ScreenKind // sm:  >= 576px
case object PhoneScreen extends ScreenKind // sx: < 576px

sealed trait UsageKind {
}
case object ListUsage extends UsageKind
case object DetailUsage extends UsageKind
case object CreateUsage extends UsageKind
case object UpdateUsage extends UsageKind
case object DeleteUsage extends UsageKind
case object InvokeUsage extends UsageKind

case class OperationScreenEntityUsageSchemaRule(
  rules: Map[OperationMode, ScreenEntityUsageSchemaRule],
  defaultRule: ScreenEntityUsageSchemaRule
) {
  // def get(p: OperationMode) = rules.get(p)
  def apply(p: OperationMode) = rules.get(p) getOrElse defaultRule
}
object OperationScreenEntityUsageSchemaRule {
  val empty = OperationScreenEntityUsageSchemaRule(Map.empty, ScreenEntityUsageSchemaRule.empty)

  def create(p: (OperationMode, ScreenEntityUsageSchemaRule), ps: (OperationMode, ScreenEntityUsageSchemaRule)*): OperationScreenEntityUsageSchemaRule =
    OperationScreenEntityUsageSchemaRule((p +: ps).toMap, p._2)

  def create(p: ScreenEntityUsageSchemaRule, ps: (OperationMode, ScreenEntityUsageSchemaRule)*): OperationScreenEntityUsageSchemaRule =
    OperationScreenEntityUsageSchemaRule(ps.toMap, p)
}

case class ScreenEntityUsageSchemaRule(
  rules: Map[ScreenKind, EntityUsageSchemaRule],
  defaultRule: EntityUsageSchemaRule
) {
  // def get(p: ScreenKind) = rules.get(p)
  def apply(p: ScreenKind) = rules.get(p) getOrElse defaultRule
}
object ScreenEntityUsageSchemaRule {
  val empty = ScreenEntityUsageSchemaRule(Map.empty, EntityUsageSchemaRule.empty)

  def create(p: (ScreenKind, EntityUsageSchemaRule), ps: (ScreenKind, EntityUsageSchemaRule)*): ScreenEntityUsageSchemaRule =
    ScreenEntityUsageSchemaRule((p +: ps).toMap, p._2)

  def create(p: EntityUsageSchemaRule, ps: (ScreenKind, EntityUsageSchemaRule)*): ScreenEntityUsageSchemaRule =
    ScreenEntityUsageSchemaRule(ps.toMap, p)
}

case class EntityUsageSchemaRule(
  rules: Map[DomainEntityType, UsageSchemaRule],
  defaultRule: UsageSchemaRule
) {
  // def get(p: Option[DomainEntityType]): Option[UsageSchemaRule] = p.flatMap(get)
  // def get(p: DomainEntityType): Option[UsageSchemaRule] = rules.get(p)
  def apply(p: DomainEntityType) = rules.get(p) getOrElse defaultRule
}
object EntityUsageSchemaRule {
  val empty = EntityUsageSchemaRule(Map.empty, UsageSchemaRule.empty)

  def create(p: (DomainEntityType, UsageSchemaRule), ps: (DomainEntityType, UsageSchemaRule)*): EntityUsageSchemaRule =
    EntityUsageSchemaRule((p +: ps).toMap, p._2)

  def create(p: UsageSchemaRule, ps: (DomainEntityType, UsageSchemaRule)*): EntityUsageSchemaRule =
    EntityUsageSchemaRule(ps.toMap, p)
}

case class UsageSchemaRule(
  rules: Map[UsageKind, Schema],
  defaultRule: Schema
) {
  // def get(p: UsageKind) = rules.get(p)
  def apply(p: UsageKind) = rules.get(p) getOrElse defaultRule
}
object UsageSchemaRule {
  val empty = UsageSchemaRule(Map.empty, Schema.empty)

  def create(p: (UsageKind, Schema), ps: (UsageKind, Schema)*): UsageSchemaRule =
    UsageSchemaRule((p +: ps).toMap, p._2)

  def create(p: Schema, ps: (UsageKind, Schema)*): UsageSchemaRule =
    UsageSchemaRule(ps.toMap, p)
}

case class SchemaRule(
  // byOperationMode: OperationScreenEntityUsageSchemaRule,
  // byScreen: ScreenEntityUsageSchemaRule, // MediaOperationMode
  // byEntity: EntityUsageSchemaRule, // DesktopScreen
  // byUsage: UsageSchemaRule, // common
  // default: Schema // AtomFeed
  rule: OperationScreenEntityUsageSchemaRule
) {
  def resolve(p: RenderStrategy, t: Renderer.TableOrder): Schema = {
    val ctx = p.renderContext
    ctx.schema getOrElse {
      val schema = t.schema.getOrElse(IRecord.makeSchema(t.records.getOrElse(Nil)))
      (ctx.entityType orElse t.entityType).fold(schema)(resolve(p, _, schema))
    }
  }

  def resolve(p: RenderStrategy, entitytype: DomainEntityType, schema: Schema): Schema = {
    val ctx = p.renderContext
    val usagekind = ctx.usageKind getOrElse p.theme.default.usageKind
    resolve(
      ctx.operationKind,
      ctx.screenKind,
      entitytype,
      usagekind,
      schema
    )
  }

  def resolve(
    op: OperationMode,
    screen: ScreenKind,
    entitytype: DomainEntityType,
    usage: UsageKind,
    schema: Schema
  ): Schema = {
    if (true) {
      schema
    } else {
      val systemschema = rule(op)(screen)(entitytype)(usage)
      _converge(schema, systemschema)
    }
  }

  private def _converge(app: Schema, system: Schema): Schema = {
    case class Z(r: Vector[Column] = Vector.empty) {
      def +(rhs: Column) = {
        app.getColumn(rhs.name).fold(this) { c =>
          // val c1 = if (rhs.datatype == XString && c.datatype != XString)
          //   rhs.copy(datatype = c.datatype)
          // else
          //   rhs
          val c1 = c
          copy(r = r :+ c1)
        }
      }
    }
    val columns = system.columns./:(Z())(_+_).r
    app.copy(columns = columns)
  }
}
object SchemaRule {
  import SchemaBuilder._
  val empty = SchemaRule(
    OperationScreenEntityUsageSchemaRule.create(
      ScreenEntityUsageSchemaRule.create(
        EntityUsageSchemaRule.create(
          UsageSchemaRule.create(
            SchemaBuilder.create(
              CLT(PROP_DOMAIN_OBJECT_ID, "Id", XEverforthid), // TODO generic platform entity id
                                                              //      CLiT(PROP_DOMAIN_OBJECT_NAME, "Name", "名前", XString),
              CLejT(PROP_DOMAIN_OBJECT_TITLE, "Title", "タイトル", XString),
              //      CLejT(PROP_DOMAIN_OBJECT_SUBTITLE, "Sub Title", "サブタイトル", XString),
              //      CLejT(PROP_DOMAIN_OBJECT_SUMMARY, "Summary", "概要", XString),
              CLejT(PROP_DOMAIN_OBJECT_CONTENT, "Content", "内容", XString),
              CLejT(PROP_DOMAIN_OBJECT_IMAGE_ICON, "Icon", "アイコン", XImageLink), // TODO XImage
              CLejT(PROP_DOMAIN_OBJECT_IMAGE_PRIMARY, "Image", "画像", XImageLink) // TODO XImage
                                                                                  //      CLejT(PROP_DOMAIN_OBJECT_IMAGE_SECONDARY, "Image", "画像", XImageLink)
            )
          )
        )
      )
    )
  )
}

/*
 * Partial
 */
sealed trait PartialKind extends NamedValueInstance
object PartialKind extends EnumerationClass[PartialKind] {
  val elements = Vector(
    HeadDefPartial,
    FootDefPartial,
    HeaderPartial,
    FooterPartial,
    NavigationPartial,
    SidebarPartial,
    ContentHeaderPartial,
    ContentPartial
  )
}
case object HeadDefPartial extends PartialKind {
  val name = "headDef"
}
case object FootDefPartial extends PartialKind {
  val name = "footDef"
}
case object HeaderPartial extends PartialKind {
  val name = "header"
}
case object FooterPartial extends PartialKind {
  val name = "footer"
}
case object NavigationPartial extends PartialKind {
  val name = "navigation"
}
case object SidebarPartial extends PartialKind {
  val name = "sidebar"
}
case object ContentHeaderPartial extends PartialKind {
  val name = "contentHeader"
}
case object ContentPartial extends PartialKind {
  val name = "content"
}

case class Partials(
  partials: Map[PartialKind, PartialView] = Map.empty,
  byLayout: Map[LayoutKind, Map[PartialKind, PartialView]] = Map.empty
) {
  private def _get(p: PartialKind): Option[PartialView] = partials.get(p)
  def get(l: LayoutKind, p: PartialKind): Option[PartialView] =
    byLayout.get(l).flatMap(_.get(p)) orElse _get(p)
  def get(l: Option[LayoutKind], p: PartialKind): Option[PartialView] =
    l.fold(_get(p))(get(_, p))
  def headDef(l: LayoutKind): Option[PartialView] = get(l, HeadDefPartial)
  def headDef(l: Option[LayoutKind]): Option[PartialView] = get(l, HeadDefPartial)
  def footDef(l: LayoutKind): Option[PartialView] = get(l, FootDefPartial)
  def footDef(l: Option[LayoutKind]): Option[PartialView] = get(l, FootDefPartial)
  def header(l: LayoutKind): Option[PartialView] = get(l, HeaderPartial)
  def header(l: Option[LayoutKind]): Option[PartialView] = get(l, HeaderPartial)
  def footer(l: LayoutKind): Option[PartialView] = get(l, FooterPartial)
  def footer(l: Option[LayoutKind]): Option[PartialView] = get(l, FooterPartial)
  def navigation(l: LayoutKind): Option[PartialView] = get(l, NavigationPartial)
  def navigation(l: Option[LayoutKind]): Option[PartialView] = get(l, NavigationPartial)
  def sidebar(l: LayoutKind): Option[PartialView] = get(l, SidebarPartial)
  def sidebar(l: Option[LayoutKind]): Option[PartialView] = get(l, SidebarPartial)
  def contentHeader(l: LayoutKind): Option[PartialView] = get(l, ContentHeaderPartial)
  def contentHeader(l: Option[LayoutKind]): Option[PartialView] = get(l, ContentHeaderPartial)
  def content(l: LayoutKind): Option[PartialView] = get(l, ContentPartial)
  def content(l: Option[LayoutKind]): Option[PartialView] = get(l, ContentPartial)

  def complement(rhs: Partials): Partials = Partials(
    MapUtils.complement(partials, rhs.partials),
    MapUtils.complement(byLayout, rhs.byLayout)
  )
  def complements(rhs: Seq[Partials]): Partials =
    rhs./:(this)(_ complement _)
}
object Partials {
  val empty = Partials()
}

case class Pages(
  pages: Map[PathName, View]
) {
  def get(p: String): Option[View] = pages.get(PathName(p))

  def complement(rhs: Pages): Pages = Pages(
    MapUtils.complement(pages, rhs.pages)
  )

  def complements(rhs: Seq[Pages]): Pages =  rhs./:(this)(_ complement _)
}
object Pages {
  val empty = Pages(Map.empty)
}

case class Components(
  components: Vector[Slot]
) {
}
object Components {
  val empty = Components(Vector.empty)
}

case class RenderContext(
  scope: RenderScope,
  size: Option[RenderSize],
  gridContext: Option[GridContext],
  operationKind: OperationMode,
  screenKind: ScreenKind,
  usageKind: Option[UsageKind],
  tableKind: Option[TableKind],
  cardKind: Option[CardKind],
  cardKindInGrid: Option[CardKind],
  sectionLevel: Option[Int],
  entityType: Option[DomainEntityType],
  schema: Option[Schema],
  dataHref: Option[URI],
  epilogue: Option[EpilogueContext],
  formatter: FormatterContext
) {
  def withScopeHtml = copy(scope = Html)
  def withScopeSection = copy(scope = Section)
  def withScopeContent = copy(scope = Content)
  def withSizeTiny = copy(size = Some(TinySize))
  def sectionDown = {
    val sl: Option[Int] = sectionLevel.map(_ + 1) orElse Some(1)
    copy(sectionLevel = sl)
  }
  def withGridContext(p: GridContext) = copy(gridContext = Some(p))
  def withEntityType(p: Option[DomainEntityType]) = copy(entityType = p)
  def withEntityType(p: DomainEntityType) = copy(entityType = Some(p))
  def withUsageKind(p: UsageKind) = copy(usageKind = Some(p))
  def withTableKind(p: TableKind) = copy(tableKind = Some(p))
  def withCardKindInGrid(p: CardKind) = copy(cardKindInGrid = Some(p))
  def withEpilogue = copy(epilogue = Some(new EpilogueContext()))
  def withFormatter(p: FormatterContext) = copy(formatter = p)

  def addJavaScriptInFooter(p: String): Unit = epilogue.
    map(_.addJavaScript(p)).
    getOrElse(RAISE.noReachDefect)

  def uri(id: DomainEntityId): URI = uri(id.entityType, id.id)
  def uri(base: URI, id: DomainObjectId): URI = {
    val s = base.toString
    val b = StringUtils.toPathnameBody(s)
    StringUtils.getSuffix(s).fold(
      new URI(s"${b}/${id.presentationId}")
    )(suffix =>
      new URI(s"${b}/${id.presentationId}.${suffix}"))
  }
  def uri(base: DomainEntityType, id: DomainObjectId): URI = new URI(s"${base.name}/${id.presentationId}${suffix}")

  def suffix: String = ".html" // TODO

  // effect
  def generateId() = java.util.UUID.randomUUID().toString

  object label {
    def commandSubmit: I18NElement = I18NElement("Execute", "実行")
    def buttonSearch = I18NElement("Search", "検索")
    def placeholderStart = I18NElement("Start", "開始")
    def placeholderEnd = I18NElement("End", "終了")
  }

  // protected def domain_entity_id(entitytype: DomainEntityType, id: Any): DomainEntityId =
  //   DomainEntityId(entitytype, StringDomainObjectId(id.toString, None, None)) // TODO
}
object RenderContext {
  val empty = RenderContext(
    Html,
    None,
    None,
    MediaOperationMode,
    DesktopScreen,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    FormatterContext.default
  )
}

// case class FormatterContext(
//   datetime: DateTimeFormatter,
//   date: DateTimeFormatter,
//   time: DateTimeFormatter
// ) {
//   def withLocale(locale: Locale) = FormatterContext(
//     datetime.withLocale(locale),
//     date.withLocale(locale),
//     time.withLocale(locale)
//   )
// }
// object FormatterContext {
//   val default = FormatterContext(
//     DateTimeFormat.mediumDateTime().withLocale(Locale.ENGLISH),
//     DateTimeFormat.mediumDate().withLocale(Locale.ENGLISH),
//     DateTimeFormat.mediumTime().withLocale(Locale.ENGLISH)
//   )

//   def create(locale: Locale, style: String): FormatterContext =
//     FormatterContext(
//       DateTimeFormat.forStyle(style).withLocale(locale),
//       DateTimeFormat.forStyle(style).withLocale(locale),
//       DateTimeFormat.forStyle(style).withLocale(locale)
//     )

//   def createStyle(style: String): FormatterContext =
//     FormatterContext(
//       DateTimeFormat.forStyle(style),
//       DateTimeFormat.forStyle(style),
//       DateTimeFormat.forStyle(style)
//     )
// }

// case class ViewContext(
//   engine: ViewEngine,
//   parcel: Parcel
// ) {
//   def isMatch(e: ViewEngine, p: Parcel) = engine == e && parcel == p
// }

class EpilogueContext {
  private val _javascripts = mutable.ArrayBuffer.empty[String]

  def addJavaScript(p: String): Unit = _javascripts += p

  def getScriptElement: Option[Elem] =
    if (_javascripts.isEmpty)
      None
    else
      Some(_make_script_tag())

  private def _make_script_tag() = <script type="text/javascript">{"""
  //<![CDATA[
    $(function(){
%s
    });
  //]]>
""".format(_javascripts.mkString("\n"))
        }</script>
}
