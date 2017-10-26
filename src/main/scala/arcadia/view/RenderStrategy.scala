package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import java.util.Locale
import java.net.URI
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.record.v2.util.{SchemaBuilder, RecordUtils}
import org.goldenport.value._
import org.goldenport.util.MapUtils
import arcadia._
import arcadia.context._
import arcadia.domain._
import arcadia.view.ViewEngine._

/*
 * @since   Jul. 31, 2017
 *  version Aug. 29, 2017
 *  version Sep. 27, 2017
 * @version Oct. 25, 2017
 * @author  ASAMI, Tomoharu
 */
case class RenderStrategy(
  locale: Locale,
  theme: RenderTheme,
  schema: SchemaRule,
  application: WebApplicationRule,
  partials: Partials,
  components: Components,
  renderContext: RenderContext,
  viewContext: Option[ViewContext]
) {
  def scope = renderContext.scope
  def size = renderContext.size getOrElse NormalSize
  def tableKind = renderContext.tableKind getOrElse theme.default.tableKind
  def tableKind(p: Option[TableKind]) = renderContext.tableKind orElse p getOrElse theme.default.tableKind
  def cardKind = renderContext.cardKind getOrElse theme.default.cardKind
  def isLogined = executeOption(_.isLogined) getOrElse false
  def getOperationName: Option[String] = executeOption(_.getOperationName).flatten

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

  def withViewContext(engine: ViewEngine, parcel: Parcel) = copy(viewContext = Some(ViewContext(engine, parcel)))
  def withThemePartials(t: RenderTheme, p: Partials) = copy(
    theme = t,
    partials = p
  )
  def withApplicationRule(p: WebApplicationRule) = copy(application = p)
  def withEntityType(p: Option[DomainEntityType]) = copy(renderContext = renderContext.withEntityType(p))
  def withEntityType(p: DomainEntityType) = copy(renderContext = renderContext.withEntityType(p))
  def withUsageKind(p: UsageKind) = copy(renderContext = renderContext.withUsageKind(p))

  def forComponent(engine: ViewEngine, parcel: Parcel) = forView(engine, parcel)
  def forView(engine: ViewEngine, parcel: Parcel) =
    if (viewContext.fold(false)(_.isMatch(engine, parcel)))
      this
    else
      copy(viewContext = Some(ViewContext(engine, parcel)))

  def getEntityType = renderContext.entityType

  def execute[T](pf: ExecutionContext => T): T =
    executeOption(pf) getOrElse RAISE.noReachDefect

  def executeOption[T](pf: ExecutionContext => T): Option[T] =
    viewContext.flatMap(_.parcel.executeOption(pf))

  def resolveSchema(p: Renderer.TableOrder): Schema = schema.resolve(this, p)

  def resolveSchema(entitytype: DomainEntityType, s: Schema): Schema = schema.resolve(this, entitytype, s)

  def format(column: Column, rec: Record): String = {
    rec.getOne(column.name).map {
      case m => m.toString // TODO
    }.getOrElse("")
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

sealed trait RenderTheme extends ClassNamedValueInstance {
  protected def name_Suffix = "Theme"
  object default {
    def noImageIcon: URI = new URI("assets/img/no-image-icon.png")
    def noUserImageIcon: URI = new URI("assets/img/no-user-image-icon.png")
    def usageKind: UsageKind = default_UsageKind
    def tableKind: TableKind = default_TableKind
    def cardKind: CardKind = default_CardKind
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
      import org.goldenport.Strings.blankopt
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
      view.strategy.application.feature_list.
        map(features).
        getOrElse(Group(Nil))
    }
  }
  object navigation {
    def content(view: ViewModel): Node = navigation_Content(view)
  }

  protected def default_UsageKind: UsageKind = ListUsage
  protected def default_TableKind: TableKind = ListTable
  protected def default_CardKind: CardKind = BootstrapCard
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
    NowUiKitTheme
  )
}

sealed trait BootstrapRenderThemaBase extends RenderTheme {
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
    <div class="content table-responsive table-full-width">
      {body}
    </div>

  private def _table_container_list(body: => Node): Node = {body}

  private def _table_container_grid(body: => Node): Node = {body}

  private def _table_container_card(body: => Node): Node = 
    <div class="content table-responsive table-full-width">
      {body}
    </div>

  private def _table_container_dashboard(body: => Node): Node = 
    <div class="content table-responsive table-full-width arcadia-dashboard">
      {body}
    </div>

  override protected def table_CssClass_Table(p: Renderer.Table): String = "table table-hover" // table, table-striped, table-bordered, table-hover, table-condensed

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

trait Bootstrap3RenderThemaBase extends BootstrapRenderThemaBase {
}

trait Bootstrap4RenderThemaBase extends BootstrapRenderThemaBase {
}

case object PlainTheme extends RenderTheme {
}
case object PaperDashboardTheme extends Bootstrap3RenderThemaBase {
  override protected def default_TableKind: TableKind = StandardTable
  override protected def default_CardKind: CardKind = PaperDashboardCard
  override protected def table_CssClass_TheadTh(p: Renderer.TableColumn): String =
    p.size.cssClass
}
case object MatrialKitTheme extends Bootstrap3RenderThemaBase {
}
case object NowUiKitTheme extends Bootstrap4RenderThemaBase {
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
}
object CardKind extends EnumerationClass[CardKind] {
  val elements = Vector(
    BootstrapCard, // Bootstrap4
    PaperDashboardCard // Creative-Tim PaperDashboard
  )
}
case object BootstrapCard extends CardKind {
  val name = "bootstrap"
}
case object PaperDashboardCard extends CardKind {
  val name = "paper-dashboard"
}

sealed trait OperationMode {
}
case object MediaOperationMode extends OperationMode
case object ConsoleOperationMode extends OperationMode

sealed trait ScreenKind {
}
case object WebScreen extends ScreenKind
case object TabletScreen extends ScreenKind
case object PhoneScreen extends ScreenKind

sealed trait UsageKind {
}
case object ListUsage extends UsageKind
case object DetailUsage extends UsageKind
case object CreateUsage extends UsageKind
case object UpdateUsage extends UsageKind
case object DeleteUsage extends UsageKind

case class OperationScreenEntityUsageSchemaRule(rules: Map[OperationMode, ScreenEntityUsageSchemarRule]) {
  def get(p: OperationMode) = rules.get(p)
}
object OperationScreenEntityUsageSchemaRule {
  val empty = OperationScreenEntityUsageSchemaRule(Map.empty)
}

case class ScreenEntityUsageSchemarRule(rules: Map[ScreenKind, EntityUsageSchemaRule]) {
  def get(p: ScreenKind) = rules.get(p)
}
object ScreenEntityUsageSchemarRule {
  val empty = ScreenEntityUsageSchemarRule(Map.empty)
}

case class EntityUsageSchemaRule(rules: Map[DomainEntityType, UsageSchemaRule]) {
  def get(p: Option[DomainEntityType]): Option[UsageSchemaRule] = p.flatMap(get)
  def get(p: DomainEntityType): Option[UsageSchemaRule] = rules.get(p)
}
object EntityUsageSchemaRule {
  val empty = EntityUsageSchemaRule(Map.empty)
}

case class UsageSchemaRule(rules: Map[UsageKind, Schema]) {
  def get(p: UsageKind) = rules.get(p)
}
object UsageSchemaRule {
  val empty = UsageSchemaRule(Map.empty)

  def create(p: (UsageKind, Schema), ps: (UsageKind, Schema)*): UsageSchemaRule =
    UsageSchemaRule((p +: ps).toMap)
}

case class SchemaRule(
  byOperationMode: OperationScreenEntityUsageSchemaRule,
  byScreen: ScreenEntityUsageSchemarRule, // MediaOperationMode
  byEntity: EntityUsageSchemaRule, // WebScreen
  byUsage: UsageSchemaRule, // common
  default: Schema // AtomFeed
) {
  def resolve(p: RenderStrategy, t: Renderer.TableOrder): Schema = {
    val ctx = p.renderContext
    ctx.schema getOrElse {
      val schema = t.schema.getOrElse(RecordUtils.buildSchema(t.records.getOrElse(Nil)))
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
    val byscreen = byOperationMode.get(op) getOrElse byScreen
    val byentity = byscreen.get(screen) getOrElse byEntity
    val byusage = byentity.get(entitytype) getOrElse byUsage
    val systemschema = byusage.get(usage) getOrElse default
    _converge(schema, systemschema)
  }

  private def _converge(app: Schema, system: Schema): Schema = {
    case class Z(r: Vector[Column] = Vector.empty) {
      def +(rhs: Column) = {
        system.getColumn(rhs.name).fold(this) { c =>
          val c1 = if (rhs.datatype == XString && c.datatype != XString)
            rhs.copy(datatype = c.datatype)
          else
            rhs
          copy(r = r :+ c1)
        }
      }
    }
    val columns = app.columns./:(Z())(_+_).r
    app.copy(columns = columns)
  }
}
object SchemaRule {
  import SchemaBuilder._
  val empty = SchemaRule(
    OperationScreenEntityUsageSchemaRule.empty,
    ScreenEntityUsageSchemarRule.empty,
    EntityUsageSchemaRule.empty,
    UsageSchemaRule.empty,
    SchemaBuilder.create(
      CLT(PROP_DOMAIN_OBJECT_ID, "Id", XEverforthid), // TODO generic platform entity id
//      CLiT(PROP_DOMAIN_OBJECT_NAME, "Name", "名前", XString),
      CLiT(PROP_DOMAIN_OBJECT_TITLE, "Title", "タイトル", XString),
//      CLiT(PROP_DOMAIN_OBJECT_SUBTITLE, "Sub Title", "サブタイトル", XString),
//      CLiT(PROP_DOMAIN_OBJECT_SUMMARY, "Summary", "概要", XString),
      CLiT(PROP_DOMAIN_OBJECT_CONTENT, "Content", "内容", XString),
      CLiT(PROP_DOMAIN_OBJECT_IMAGE_ICON, "Icon", "アイコン", XImageLink), // TODO XImage
      CLiT(PROP_DOMAIN_OBJECT_IMAGE_PRIMARY, "Image", "画像", XImageLink) // TODO XImage
//      CLiT(PROP_DOMAIN_OBJECT_IMAGE_SECONDARY, "Image", "画像", XImageLink)
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
case object ContentPartial extends PartialKind {
  val name = "content"
}

case class Partials(
  partials: Map[PartialKind, PartialView]
) {
  def get(p: PartialKind): Option[PartialView] = partials.get(p)
  def headDef: Option[PartialView] = get(HeadDefPartial)
  def footDef: Option[PartialView] = get(FootDefPartial)
  def header: Option[PartialView] = get(HeaderPartial)
  def footer: Option[PartialView] = get(FooterPartial)
  def navigation: Option[PartialView] = get(NavigationPartial)
  def sidebar: Option[PartialView] = get(SidebarPartial)
  def content: Option[PartialView] = get(ContentPartial)

  def complement(rhs: Partials): Partials = Partials(
    MapUtils.complement(partials, rhs.partials)
  )
  def complements(rhs: Seq[Partials]): Partials =
    rhs./:(this)(_ complement _)
}
object Partials {
  val empty = Partials(Map.empty)
}

case class Components(
  components: Vector[ComponentView]
) {
  def toSlots: Vector[ViewEngine.Slot] = components.map(x => x.guard -> x).map(Slot(_))
}
object Components {
  val empty = Components(Vector.empty)
}

case class RenderContext(
  scope: RenderScope,
  size: Option[RenderSize],
  operationKind: OperationMode,
  screenKind: ScreenKind,
  usageKind: Option[UsageKind],
  tableKind: Option[TableKind],
  cardKind: Option[CardKind],
  sectionLevel: Option[Int],
  entityType: Option[DomainEntityType],
  schema: Option[Schema]
) {
  def withScopeHtml = copy(scope = Html)
  def withScopeSection = copy(scope = Section)
  def withScopeContent = copy(scope = Content)
  def withSizeTiny = copy(size = Some(TinySize))
  def sectionDown = {
    val sl: Option[Int] = sectionLevel.map(_ + 1) orElse Some(1)
    copy(sectionLevel = sl)
  }
  def withEntityType(p: Option[DomainEntityType]) = copy(entityType = p)
  def withEntityType(p: DomainEntityType) = copy(entityType = Some(p))
  def withUsageKind(p: UsageKind) = copy(usageKind = Some(p))
}
object RenderContext {
  val empty = RenderContext(
    Html,
    None,
    MediaOperationMode,
    WebScreen,
    None,
    None,
    None,
    None,
    None,
    None
  )
}

case class ViewContext(
  engine: ViewEngine,
  parcel: Parcel
) {
  def isMatch(e: ViewEngine, p: Parcel) = engine == e && parcel == p
}
