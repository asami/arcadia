package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import scala.util.control.NonFatal
import java.util.Locale
import java.net.{URI, URL}
import org.joda.time.DateTime
import org.goldenport.exception.RAISE
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => _, _}
import org.goldenport.record.v2.util.{RecordUtils, SchemaBuilder}
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.xml.XmlUtils
import org.goldenport.xml.dom.DomUtils
import org.goldenport.trace.TraceContext
import org.goldenport.util.{DateTimeUtils, DateUtils, StringUtils, SeqUtils}
import arcadia.Parcel
import arcadia.model._
import arcadia.domain._
import arcadia.context._
import arcadia.controller.Controller.PROP_REDIRECT

/*
 * @since   Aug.  1, 2017
 *  version Sep. 26, 2017
 *  version Oct. 31, 2017
 *  version Nov. 22, 2017
 *  version Dec. 19, 2017
 *  version Jan. 21, 2018
 *  version Feb. 26, 2018
 *  version Mar. 21, 2018
 *  version Apr. 22, 2018
 *  version May.  6, 2018
 *  version Jul. 23, 2018
 *  version Aug. 31, 2018
 *  version Sep.  1, 2018
 *  version Nov.  7, 2018
 *  version Aug.  5, 2019
 *  version Apr. 15, 2020
 * @version Mar. 30, 2023
 * @author  ASAMI, Tomoharu
 */
abstract class Renderer(
  val strategy: RenderStrategy,
  pageName: Option[I18NString] = None,
  headline: Option[I18NElement] = None,
  title: Option[I18NElement] = None,
  val caption: Option[I18NElement] = None
) extends RendererTablePart with RendererGridPart with RendererListPart
    with RendererCardPart with RendererTabsPart with RendererFormPart {
  import Renderer._
  protected lazy val l10n_page_name = pageName.map(string)
  protected lazy val l10n_headline = headline.map(nodeseq)
  protected lazy val l10n_title = title.map(nodeseq)
  protected lazy val l10n_caption = caption.map(nodeseq)
  protected lazy val effective_page_name = l10n_page_name orElse l10n_headline orElse l10n_title orElse l10n_caption
  protected lazy val effective_headline = l10n_headline orElse l10n_headline
  def locale = strategy.locale
  def theme = strategy.theme
  protected lazy val theme_head = theme.head
  protected lazy val theme_table = theme.table
  protected lazy val theme_grid = theme.grid
  protected lazy val theme_card = theme.card
  protected lazy val render_context = strategy.renderContext

  def apply: NodeSeq = strategy.scope match {
    case Html => render_html
    case Body => render_body
    case Section => render_section
    case Content => render_content
  }

  def execute[T](s: RenderStrategy)(body: Renderer => T): T = {
    def go = render_Content
    body(new Renderer(s, pageName, headline, title, caption) {
      protected def render_Content: NodeSeq = go
    })
  }

  protected def generate_id() = strategy.generateId()

  protected def add_javascript_in_footer(p: String): Unit = strategy.addJavaScriptInFooter(p)

  protected def get_html_suffix: Option[String] = Some("html")
  protected def make_html_uri(p: String): URI = {
    val a = get_html_suffix.fold(p) { suffix =>
      if (_has_suffix(p))
        p
      else
        s"${p}.${suffix}"
    }
    new URI(a)
  }

  private def _has_suffix(p: String): Boolean =
    (p.lastIndexOf('-'), p.lastIndexOf('.')) match {
      case (-1, -1) => false
      case (-1, r) => true
      case (l, r) => r > l
    }

  protected lazy val button_search = strategy.label.buttonSearch.apply(locale)
  protected lazy val placeholder_start = strategy.label.placeholderStart.toI18NString.apply(locale)
  protected lazy val placeholder_end = strategy.label.placeholderEnd.toI18NString.apply(locale)

  protected def render_html = <html>{render_head}{render_body}</html>
  protected def render_head = <head>
  {head_charset}
  {head_keywords}
  {head_description}
  {head_robots}
  {head_author}
  {effective_page_name.map(x => <title>{x}</title>).toList}
  {head_theme}
  </head>
  protected def render_body = <body>{render_header}{effective_headline.map(x => <h1>{x}</h1>).toList}{render_side_bar}{render_section}</body>
  protected def render_header = Group(Nil)
  protected def render_side_bar = Group(Nil)
  protected def render_section: NodeSeq = {
    val a = Group(l10n_title.map(x => <h2>{x}</h2>).toList)
    val b = render_content
    a ++ b
  }
  protected def render_content = render_Content

  protected def render_Content: NodeSeq

  protected def head_charset: Node = theme_head.charset(strategy)
  protected def head_keywords: Node = theme_head.keywords(strategy)
  protected def head_description: Node = theme_head.description(strategy)
  protected def head_robots: Node = theme_head.robots(strategy)
  protected def head_author: Node = theme_head.author(strategy)
  protected def head_theme: Node = theme_head.theme(strategy)

  protected def string(s: I18NString): String = s.apply(locale)
  protected def node(s: I18NString): Node = Text(s.apply(locale))
  protected def nodeseq(s: I18NElement): NodeSeq = s.apply(locale)

  protected def error(
    code: Int,
    message: Option[I18NElement],
    exception: Option[Throwable],
    invalid: Option[Invalid],
    topUri: Option[URI],
    backUri: Option[URI],
    trace: Option[TraceContext]
  ): NodeSeq = {
    import SchemaBuilder._

    val msg: Option[I18NElement] = message orElse invalid.map(_.i18nMessage.toI18NElement)

    val schema = SchemaBuilder.create(
      CL("code", "Code"),
      CL("message", "Message"),
      CLT("topuri", "Top URI", XLink),
      CLT("backuri", "Back URI", XLink),
      CL("exception_message", "Exception Message"),
      CLT("exception_stack", "Exception Stack", XText),
      CLT("call_tree", "Call Tree", XText)
    )

    val rec = Record.dataOption(
      "code" -> Some(code),
      "message" -> msg.map(_(locale)),
      "topuri" -> topUri,
      "backuri" -> backUri,
      "exception_message" -> exception.flatMap(x => Option(x.getMessage)),
      "exception_stack" -> exception.map(x => StringUtils.makeStack(x)),
      "call_tree" -> trace.map(_.showTree)
    )
    property_sheet(schema, rec)
  }

  protected def widget(p: WidgetModel): NodeSeq = {
    val r = strategy.viewContext.flatMap { x =>
      val a = x.parcel.withModel(p)
      strategy.viewContext.flatMap(_.engine.renderComponentOption(a))
    }
    r.getOrElse(<div class="arcaida_warning">{s"No widget ${p.name}"}</div>)
  }

  protected def text(p: String): Text = Text(p)

  protected def empty_block: Elem = <div></div> // TODO

  protected def seq(head: Option[NodeSeq], tail: Option[NodeSeq]*): Seq[NodeSeq] =
    (head :: tail.toList).flatten

  protected def h2(p: Option[I18NString]): Node = p.map(x => <h2>string(x)</h2>).getOrElse(Group(Nil))

  private def _get_href_base(p: IRecord): Option[URI] =
    (
      p.getString(KEY_DOMAIN_OBJECT_HREF_BASE) orElse
      p.getString(KEY_DOMAIN_OBJECT_ENTITYTYPE)
    ).map(make_html_uri)

  protected def get_link(table: Table, rec: IRecord): Option[DomainEntityLink] = {
    val id = DomainEntityId.get(rec, table.entityType)
    (
      id,
      table.dataHref orElse _get_href_base(rec)
    ) match {
      case (None, _) => None
      case (Some(id), href) => Some(DomainEntityLink(id, href))
    }
  }

  protected def get_link(rec: IRecord): Option[DomainEntityLink] = {
    val id = DomainEntityId.get(rec)
    (
      id,
      _get_href_base(rec)
    ) match {
      case (None, _) => None
      case (Some(id), href) => Some(DomainEntityLink(id, href))
    }
  }

  protected def table_data_url(p: Table, record: IRecord): Option[Link] = {
    def base = (p.dataHref.map(x => StringUtils.toPathnameBody(x.toString)) orElse p.entityType.map(_.name)).map(make_html_uri) orElse {
      _get_href_base(record)
    }
    DomainObjectId.get(record, p.entityType) flatMap {
      case m: DomainEntityId => Some(DomainEntityLink(m, base))
      case m: DomainObjectId => base.map(x => DomainObjectLink(m, Some(x)))
    }
  }

  protected final def table_data_url_string(p: Table, record: IRecord): Option[String] =
    table_data_url(p, record).map(_.dataHref(strategy.renderContext).toString)

  protected def tabular(schema: Option[Schema], records: Seq[IRecord]): NodeSeq =
    schema.fold(tabular(records))(tabular(_, records))

  protected def tabular(records: Seq[IRecord]): NodeSeq = {
    val schema = build_schema(records)
    tabular(schema, records)
  }

  protected def tabular(schema: Schema, records: Seq[IRecord]): NodeSeq = {
    val t = TableWithRecords(TabularTable, strategy.size, schema, records)
    <table class={theme_table.css.table(t.table)}>{
    seq(
      caption.map(x => <caption class={theme_table.css.tbody(t.table)}>{nodeseq(x)}</caption>),
      Some(table_records(t))
    )
    }</table>
  }

  // Property
  protected def property_table(schema: Option[Schema], records: Seq[IRecord], datahref: Option[URI]): NodeSeq =
    table(PropertyTable, schema, records, datahref)

  protected def property_table(schema: Schema, records: Seq[IRecord], datahref: Option[URI]): NodeSeq =
    table(PropertyTable, schema, records, datahref)

  protected def entity_property_sheet(
    entitytype: DomainEntityType,
    schema: Option[Schema],
    record: IRecord
  ): NodeSeq = schema.fold(
    property_sheet(schema, record)
  )(s =>
    property_sheet(
      strategy.withUsageKind(DetailUsage).resolveSchema(entitytype, s),
      record
    )
  )

  protected def property_sheet_list(records: Seq[IRecord]): NodeSeq = {
    import SchemaBuilder._
    val schema = SchemaBuilder.create(
      CT("no", XInt),
      CT("data", XRecordInstance)
    )
    val t = Table(PropertyTable, strategy.size, schema)
    <table class={theme_table.css.table(t)}>
      <tbody>{
        for ((x, i) <- records.zipWithIndex) yield {
          <tr class={theme_table.css.tbodyTr(t)}>
            <td>{i + 1}</td>
            <td>{property_sheet(x)}</td>
          </tr>
        }
      }</tbody>
    </table>
  }

  protected def property_sheet(schema: Option[Schema], record: IRecord): NodeSeq =
    schema.fold(property_sheet(record))(property_sheet(_, record))

  protected def property_sheet(record: IRecord): NodeSeq = {
    val schema = build_schema(Vector(record))
    property_sheet(schema, record)
  }

  protected def property_sheet(schema: Schema, record: IRecord): NodeSeq = {
    val t = Table(PropertyTable, strategy.size, schema)
    <table class={theme_table.css.table(t)}>
      <tbody>{
        for (c <- schema.columns) yield {
          <tr class={theme_table.css.tbodyTr(t)}>{
            val tc = t.tableColumn(c)
            List(
              <th scope="row" class={theme_table.css.theadTh(tc)}>{c.label(locale)}</th>,
              table_data(tc, record)
            )
          }</tr>
        }
      }</tbody>
    </table>
  }

  protected def property_sheet_confirm(): NodeSeq = RAISE.notImplementedYetDefect

  protected def get_title(rec: IRecord): Option[I18NElement] =
    rec.getString(KEY_DOMAIN_OBJECT_TITLE).map(I18NElement.parse)

  protected def get_subtitle(rec: IRecord): Option[I18NElement] =
    rec.getString(KEY_DOMAIN_OBJECT_SUBTITLE).map(I18NElement.parse)

  protected def get_content(rec: IRecord): Option[Node] =
    rec.getString(KEY_DOMAIN_OBJECT_CONTENT).map(table_value_html)

  protected def get_content_summary(rec: IRecord): Option[Node] =
    rec.getString(KEY_DOMAIN_OBJECT_SUMMARY).map(table_value_html_summary)

  protected def picture_icon(rec: IRecord): Picture =
    rec.get(KEY_DOMAIN_OBJECT_IMAGE_ICON).fold {
      Picture.create(theme.default.noImageIcon)
    } {
      case m: URL => Picture.create(m.toURI)
      case m: URI => Picture.create(m)
      case m: String => Picture.create(new URI(m))
      case m: Picture => m
      case m => RAISE.noReachDefect
    }

  protected def carousel(
    ps: List[Picture],
    isControl: Boolean = true,
    isIndicator: Boolean = true,
    isCaption: Boolean = true
  ): Elem = {
    val id = generate_id
    <div id={s"$id"} class="carousel slide w-100 ml-auto mr-auto" data-ride="carousel">
      {
        if (isIndicator)
          <ol class="carousel-indicators" role="listbox"> {
            for ((x, i) <- ps.zipWithIndex) yield {
              if (i == 0)
                <li data-target={s"#${id}"} data-slide-to="0" class="active"></li>
              else
                <li data-target={s"#${id}"} data-slide-to={s"$i"}></li>
            }
          } </ol>
        else
          Group(Nil)
      }
      <div class="carousel-inner"> {
        for ((x, i) <- ps.zipWithIndex) yield {
          val c = if (i == 0) "carousel-item active" else "carousel-item"
          <div class={c}>
  	    <img class="d-block w-100" src={x.l} alt={x.alt(locale)}></img>
            {
              if (!isCaption || (x.caption.isEmpty && x.description.isEmpty))
                Group(Nil)
              else
                <div class="carousel-caption d-none d-md-block"> {
                  List(
                    x.caption.map(c => <h3>{c(locale)}</h3>),
                    x.description.map(d => <p>{d(locale)}</p>)
                  ).flatten
                } </div>
            }
          </div>
        }
      } </div>
      {
        if (isControl) {
          <a class="carousel-control-prev" href={s"#${id}"} role="button" data-slide="prev">
            <span class="carousel-control-prev-icon" aria-hidden="true"></span>
            <span class="sr-only">Previous</span>
	  </a>
	  <a class="carousel-control-next" href={s"#${id}"} role="button" data-slide="next">
            <span class="carousel-control-next-icon" aria-hidden="true"></span>
            <span class="sr-only">Next</span>
	  </a>
        } else {
          Group(Nil)
        }
      }
    </div>
  }

  protected def banner(ps: List[Picture]): Elem =
    if (ps.length == 0)
      empty_block
    else
      _banner(ps)

  private def _banner(ps: List[Picture]): Elem = {
    val g = GridContext.banner.withTabletColumns(ps.length)
    val s: RenderStrategy = strategy.withCardKindInGrid(ImageCard).withGridContext(g)
    execute(s)(_.grid_with_content_bootstrap(ps)(picture(_)))
  }

  protected def picture(p: Picture): Elem = 
    p.href.fold {
      img(p)
    } { href =>
      <a href={href.toString} target="_blank">{img(p)}</a>
    }

  def img(p: Picture): Elem = XmlUtils.element("img",
    SeqUtils.buildTupleVector(
      Vector(
        "class" -> "card-img-top",
        "src" -> p.src.toString
      ),
      Vector(
        "alt" -> p.alt.map(node).map(_.text)
      )
    ),
    Nil
  )

  protected def badge(p: Badge): Elem = {
    val c = s"badge badge-${p.asIndicatorName}"
    <span class={c}>{p.asLabel}</span>
  }

  protected def operation_outcome(req: Request, res: Response): NodeSeq = {
    import SchemaBuilder._
    val schema = SchemaBuilder.create(
      CL("pathname", "Pathname"),
      CL("operation", "Operation"),
      CL("method", "Method"),
      CL("code", "Code")
    )

    val rec = Record.data(
      "pathname" -> req.pathname,
      "operation" -> req.operationName,
      "method" -> req.method,
      "code" -> res.code
    ) + Record.dataOption(
    )
    property_sheet(schema, rec)
  }

  /*
   * Utilities
   */
  protected final def build_schema(record: IRecord): Schema =
    IRecord.makeSchema(Vector(record))

  protected final def build_schema(rs: Seq[IRecord]): Schema =
    IRecord.makeSchema(rs)
}

object Renderer {
  case class TableOrder(
    kind: Option[TableKind],
    size: Option[RenderSize],
    schema: Option[Schema],
    entityType: Option[DomainEntityType],
    dataHref: Option[URI],
    records: Option[Seq[IRecord]]
  )
  object TableOrder {
    def apply(
      kind: TableKind,
      schema: Option[Schema],
      entitytype: DomainEntityType,
      records: Seq[IRecord]
    ): TableOrder = TableOrder(Some(kind), None, schema, Some(entitytype), None, Some(records))

    def apply(
      kind: TableKind,
      schema: Option[Schema],
      entitytype: Option[DomainEntityType],
      records: Seq[IRecord]
    ): TableOrder = TableOrder(Some(kind), None, schema, entitytype, None, Some(records))

    def apply(
      kind: Option[TableKind],
      schema: Option[Schema],
      entitytype: DomainEntityType,
      records: Seq[IRecord]
    ): TableOrder = TableOrder(kind, None, schema, Some(entitytype), None, Some(records))

    def apply(
      kind: Option[TableKind],
      schema: Option[Schema],
      entitytype: Option[DomainEntityType],
      datahref: Option[URI],
      records: Seq[IRecord]
    ): TableOrder = TableOrder(kind, None, schema, entitytype, datahref, Some(records))
  }

  case class Table(
    kind: TableKind,
    size: RenderSize,
    schema: Schema,
    entityType: Option[DomainEntityType],
    dataHref: Option[URI]
  ) {
    def tableColumn(c: Column) = TableColumn(kind, size, c)
  }
  object Table {
    def apply(
      kind: TableKind,
      size: RenderSize,
      schema: Schema
    ): Table = Table(kind, size, schema, None, None)
  }

  case class TableWithRecords(
    table: Table,
    records: Seq[IRecord]
  ) {
    def kind = table.kind
    def size = table.size
    def schema = table.schema
    def entityType = table.entityType
  }
  object TableWithRecords {
    def apply(
      kind: TableKind,
      size: RenderSize,
      schema: Schema,
      entitytype: DomainEntityType,
      records: Seq[IRecord]
    ): TableWithRecords = TableWithRecords(
      Table(kind, size, schema, Some(entitytype), None),
      records
    )

    def apply(
      kind: TableKind,
      size: RenderSize,
      schema: Schema,
      entitytype: Option[DomainEntityType],
      datahref: Option[URI],
      records: Seq[IRecord]
    ): TableWithRecords = TableWithRecords(
      Table(kind, size, schema, entitytype, datahref),
      records
    )

    def apply(
      kind: TableKind,
      size: RenderSize,
      schema: Schema,
      records: Seq[IRecord]
    ): TableWithRecords = TableWithRecords(
      Table(kind, size, schema),
      records
    )
  }

  case class TableColumn(
    kind: TableKind,
    size: RenderSize,
    column: Column
  ) {
    def name = column.name
    def datatype = column.datatype
  }

  case class TableColumnWithRecord(
    tableColumn: TableColumn,
    record: IRecord
  )

  case class InputForm(
    action: URI,
    method: Method,
    schema: Schema,
    values: IRecord,
    hiddens: Hiddens,
    submits: Submits
  ) {
    def isQuery: Boolean = method == Get
    def getValue(key: String): Option[Any] = values.get(key)
  }

  case class SearchBox(
    input: InputForm
  ) {
    def schema = input.schema
  }
  object SearchBox {
    def apply(action: URI, schema: Schema): SearchBox = {
      val hiddens = Hiddens.empty
      val button = Submit(SearchSubmitKind)
      val values = Record.empty
      val submits = Submits(button)
      val input = InputForm(
        action,
        Get,
        schema,
        values,
        hiddens,
        submits
      )
      SearchBox(input)
    }
  }
}
