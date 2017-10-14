package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import java.util.Locale
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.{Record, Schema, Column}
import org.goldenport.record.v2.util.SchemaBuilder
import org.goldenport.value._
import org.goldenport.util.MapUtils
import arcadia._
import arcadia.context._
import arcadia.domain.DomainEntityType
import arcadia.view.ViewEngine._

/*
 * @since   Jul. 31, 2017
 *  version Aug. 29, 2017
 *  version Sep. 27, 2017
 * @version Oct. 14, 2017
 * @author  ASAMI, Tomoharu
 */
case class RenderStrategy(
  scope: RenderScope,
  size: RenderSize,
  locale: Locale,
  sectionLevel: Option[Int],
  theme: RenderTheme,
  schema: SchemaRule,
  application: WebApplicationRule,
  partials: Partials,
  components: Components,
  renderContext: RenderContext,
  viewContext: Option[ViewContext]
) {
  def tableKind = renderContext.tableKind
  def isLogined = executeOption(_.isLogined) getOrElse false
  def getOperationName: Option[String] = executeOption(_.getOperationName).flatten

  def withScopeHtml = copy(scope = Html)
  def withScopeSection = copy(scope = Section)
  def withScopeSectionDown = scope match {
    case Html => copy(scope = Section)
    case Body => copy(scope = Section)
    case Section =>
      val sl: Option[Int] = sectionLevel.map(_ + 1) orElse Some(1)
      copy(sectionLevel = sl)
    case Content => this
  }
  def withScopeContent = copy(scope = Content)

  def withSizeTiny = copy(size = TinySize)

  def withViewContext(engine: ViewEngine, parcel: Parcel) = copy(viewContext = Some(ViewContext(engine, parcel)))
  def withThemePartials(t: RenderTheme, p: Partials) = copy(
    theme = t,
    partials = p
  )
  def withApplicationRule(p: WebApplicationRule) = copy(application = p)
  def withEntityType(p: Option[DomainEntityType]) = copy(renderContext = renderContext.withEntityType(p))
  def withEntityType(p: DomainEntityType) = copy(renderContext = renderContext.withEntityType(p))

  def forComponent(engine: ViewEngine, parcel: Parcel) = forView(engine, parcel)
  def forView(engine: ViewEngine, parcel: Parcel) =
    if (viewContext.fold(false)(_.isMatch(engine, parcel)))
      this
    else
      copy(viewContext = Some(ViewContext(engine, parcel)))

  def execute[T](pf: ExecutionContext => T): T =
    executeOption(pf) getOrElse RAISE.noReachDefect

  def executeOption[T](pf: ExecutionContext => T): Option[T] =
    viewContext.flatMap(_.parcel.executeOption(pf))

  def resolveSchema: Schema = schema.resolve(renderContext)

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
}
case object NormalSize extends RenderSize // 14pt
case object SmallSize extends RenderSize // 12pt
case object VerySmallSize extends RenderSize // 10pt
case object TinySize extends RenderSize // 8pt

sealed trait RenderTheme extends ClassNamedValueInstance {
  def name_Suffix = "Theme"
  object head {
    def charset(strategy: RenderStrategy): Node = <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    def keywords(strategy: RenderStrategy): Node = <meta name="keywords" content="TBD"/>
    def description(strategy: RenderStrategy): Node = <meta name="description" content="TBD"/>
    def robots(strategy: RenderStrategy): Node = <meta name="robots" content="noindex,nofollow"/>
    def author(strategy: RenderStrategy): Node = <meta name="author" content="TBD"/>
    def theme(strategy: RenderStrategy): Node = Group(Nil)
  }
  object body {
    def className(strategy: RenderStrategy): String = body_ClassName(strategy)
  }
  object table {
    def container(kind: TableKind, schema: Schema, records: Seq[Record]
, body: => Node): Node = table_Container(kind, schema, records, body)
    object className {
      import org.goldenport.Strings.blankopt
      def caption(kind: TableKind) = table_ClassName_Caption(kind)
      def table(kind: TableKind) = table_ClassName_Table(kind)
      def thead(kind: TableKind) = table_ClassName_Thead(kind)
      def tbody(kind: TableKind) = table_ClassName_Tbody(kind)
      def tfoot(kind: TableKind) = table_ClassName_Tfoot(kind)
      def theadTr(kind: TableKind) = table_ClassName_TheadTr(kind)
      def theadTh(kind: TableKind) = table_ClassName_TheadTh(kind)
      def tbodyTr(kind: TableKind) = table_ClassName_TbodyTr(kind)
      def getTbodyTr(kind: TableKind) = blankopt(table_ClassName_TbodyTr(kind))
      def tbodyTd(kind: TableKind) = table_ClassName_TbodyTd(kind)
      def tfootTr(kind: TableKind) = table_ClassName_TfootTr(kind)
      def tfootTd(kind: TableKind) = table_ClassName_TfootTd(kind)
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

  protected def body_ClassName(strategy: RenderStrategy): String = ""
  protected def table_Container(kind: TableKind, schema: Schema, records: Seq[Record]
, body: => Node): Node = body
  protected def table_ClassName_Caption(kind: TableKind): String = ""
  protected def table_ClassName_Table(kind: TableKind): String = ""
  protected def table_ClassName_Thead(kind: TableKind): String = ""
  protected def table_ClassName_Tbody(kind: TableKind): String = ""
  protected def table_ClassName_Tfoot(kind: TableKind): String = ""
  protected def table_ClassName_TheadTr(kind: TableKind): String = ""
  protected def table_ClassName_TheadTh(kind: TableKind): String = ""
  protected def table_ClassName_TbodyTr(kind: TableKind): String = ""
  protected def table_ClassName_TbodyTd(kind: TableKind): String = ""
  protected def table_ClassName_TfootTr(kind: TableKind): String = ""
  protected def table_ClassName_TfootTd(kind: TableKind): String = ""

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
  val elements = Vector(PlainTheme, PaperDashboardTheme, MatrialKitTheme, NowUiKitTheme)
}

sealed trait BootstrapRenderThemaBase extends RenderTheme {
  override protected def table_Container(kind: TableKind, schema: Schema, records: Seq[Record]
, body: => Node): Node = kind match {
    case StandardTable => _table_container_standard(body)
    case TabularTable => _table_container_card(body) // TODO
    case PropertyTable => _table_container_standard(body) // TODO
    case EntityTable => _table_container_standard(body) // TODO
    case FormTable => _table_container_standard(body) // TODO
    case DashboardTable => _table_container_card(body)
  }

  private def _table_container_standard(body: => Node): Node = // TODO
    <div class="content table-responsive table-full-width">
      {body}
    </div>

  private def _table_container_card(body: => Node): Node = 
    <div class="content table-responsive table-full-width" style="font-size:8px">
      {body}
    </div>

  override protected def table_ClassName_Table(kind: TableKind): String = "table table-hover" // table, table-striped, table-bordered, table-hover, table-condensed

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

case object PlainTheme extends RenderTheme {
}
case object PaperDashboardTheme extends BootstrapRenderThemaBase {
}
case object MatrialKitTheme extends BootstrapRenderThemaBase {
}
case object NowUiKitTheme extends BootstrapRenderThemaBase {
  override def body_ClassName(strategy: RenderStrategy): String = {
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
  def get(p: Option[DomainEntityType]): Option[UsageSchemaRule] = p.flatMap(rules.get)
}
object EntityUsageSchemaRule {
  val empty = EntityUsageSchemaRule(Map.empty)
}

case class UsageSchemaRule(rules: Map[UsageKind, Schema]) {
  def get(p: UsageKind) = rules.get(p)
}
object UsageSchemaRule {
  val empty = UsageSchemaRule(Map.empty)
}

case class SchemaRule(
  byOperationMode: OperationScreenEntityUsageSchemaRule,
  byScreen: ScreenEntityUsageSchemarRule, // MediaOperationMode
  byEntity: EntityUsageSchemaRule, // WebScreen
  byUsage: UsageSchemaRule, // common
  default: Schema // AtomFeed
) {
  def resolve(p: RenderContext): Schema = resolve(
    p.operationKind, p.screenKind, p.entityType, p.usageKind
  )

  def resolve(
    op: OperationMode,
    screen: ScreenKind,
    entitytype: Option[DomainEntityType],
    usage: UsageKind
  ): Schema = {
    val byscreen = byOperationMode.get(op) getOrElse byScreen
    val byentity = byscreen.get(screen) getOrElse byEntity
    val byusage = byentity.get(entitytype) getOrElse byUsage
    byusage.get(usage) getOrElse default
  }
}
object SchemaRule {
  val empty = SchemaRule(
    OperationScreenEntityUsageSchemaRule.empty,
    ScreenEntityUsageSchemarRule.empty,
    EntityUsageSchemaRule.empty,
    UsageSchemaRule.empty,
    SchemaBuilder.create(
      // TODO
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
  operationKind: OperationMode,
  screenKind: ScreenKind,
  usageKind: UsageKind,
  tableKind: TableKind,
  entityType: Option[DomainEntityType]
) {
  def withEntityType(p: Option[DomainEntityType]) = copy(entityType = p)
  def withEntityType(p: DomainEntityType) = copy(entityType = Some(p))
}
object RenderContext {
  val empty = RenderContext(
    MediaOperationMode,
    WebScreen,
    DetailUsage,
    StandardTable,
    None
  )
}

case class ViewContext(
  engine: ViewEngine,
  parcel: Parcel
) {
  def isMatch(e: ViewEngine, p: Parcel) = engine == e && parcel == p
}
