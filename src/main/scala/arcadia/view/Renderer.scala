package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import scala.util.control.NonFatal
import java.util.Locale
import java.net.{URI, URL}
import org.joda.time.DateTime
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.record.v2.util.{RecordUtils, SchemaBuilder}
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.xml.XmlUtils
import org.goldenport.xml.dom.DomUtils
import org.goldenport.trace.TraceContext
import org.goldenport.util.{DateTimeUtils, DateUtils, StringUtils, SeqUtils}
import arcadia.model._
import arcadia.domain._

/*
 * @since   Aug.  1, 2017
 *  version Sep. 26, 2017
 *  version Oct. 31, 2017
 *  version Nov. 22, 2017
 * @version Dec. 13, 2017
 * @author  ASAMI, Tomoharu
 */
abstract class Renderer(
  strategy: RenderStrategy,
  pageName: Option[I18NString] = None,
  headline: Option[I18NElement] = None,
  title: Option[I18NElement] = None,
  caption: Option[I18NElement] = None
) {
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

  protected def generate_id() = java.util.UUID.randomUUID().toString

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

  // TODO customizable
  protected def button_search = I18NString("Search", "検索").apply(locale)
  protected def placeholder_start = I18NString("Start", "開始").apply(locale)
  protected def placeholder_end = I18NString("End", "終了").apply(locale)

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
    topUri: Option[URI],
    backUri: Option[URI],
    trace: Option[TraceContext]
  ): NodeSeq = {
    import SchemaBuilder._
    val schema = SchemaBuilder.create(
      CL("code", "Code"),
      CL("message", "Message"),
      CLT("topuri", "Top URI", XLink),
      CLT("backuri", "Back URI", XLink),
      CL("exception_message", "Exception Message"),
      CLT("exception_stack", "Exception Stack", XText),
      CLT("call_tree", "Call Tree", XText)
    )

    val rec = Record.dataAppOption(
      "code" -> Some(code),
      "message" -> message.map(_(locale)),
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

  private def _get_href_base(p: Record): Option[URI] =
    (
      p.getConcreteString(KEY_DOMAIN_OBJECT_HREF_BASE) orElse
      p.getConcreteString(KEY_DOMAIN_OBJECT_ENTITYTYPE)
    ).map(make_html_uri)

  protected def get_link(table: Table, rec: Record): Option[DomainEntityLink] = {
    val id = DomainEntityId.get(rec, table.entityType)
    (
      id,
      table.dataHref orElse _get_href_base(rec)
    ) match {
      case (None, _) => None
      case (Some(id), href) => Some(DomainEntityLink(id, href))
    }
  }

  protected def get_link(rec: Record): Option[DomainEntityLink] = {
    val id = DomainEntityId.get(rec)
    (
      id,
      _get_href_base(rec)
    ) match {
      case (None, _) => None
      case (Some(id), href) => Some(DomainEntityLink(id, href))
    }
  }

  protected def table_data_url(p: Table, record: Record): Option[DomainEntityLink] = {
    def base = (p.dataHref.map(x => StringUtils.toPathnameBody(x.toString)) orElse p.entityType.map(_.v)).map(make_html_uri) orElse {
      _get_href_base(record)
    }
    DomainEntityId.get(record, p.entityType).map(id =>
      DomainEntityLink(id, base))
  }

  protected final def table_data_url_string(p: Table, record: Record): Option[String] =
    table_data_url(p, record).map(_.dataHref(strategy.renderContext).toString)

  protected def tabular(schema: Option[Schema], records: Seq[Record]): NodeSeq =
    schema.fold(tabular(records))(tabular(_, records))

  protected def tabular(records: Seq[Record]): NodeSeq = {
    val schema = build_schema(records)
    tabular(schema, records)
  }

  protected def tabular(schema: Schema, records: Seq[Record]): NodeSeq = {
    val t = TableWithRecords(TabularTable, strategy.size, schema, records)
    <table class={theme_table.css.table(t.table)}>{
    seq(
      caption.map(x => <caption class={theme_table.css.tbody(t.table)}>{nodeseq(x)}</caption>),
      Some(table_records(t))
    )
    }</table>
  }

  protected def table(p: TableOrder): NodeSeq = {
    val records = p.records getOrElse Nil
    val kind = p.kind getOrElse strategy.tableKind
    val size = p.size getOrElse strategy.size
    val schema = strategy.resolveSchema(p)
//    val schema = p.schema getOrElse build_schema(records)
    val entitytype = p.entityType
    val datahref = p.dataHref
    val t = TableWithRecords(kind, size, schema, entitytype, datahref, records)
    table(t)
  }

  protected def table(kind: TableKind, schema: Option[Schema], records: Seq[Record]): NodeSeq =
    schema.fold(table(records))(table(kind, _, records))

  protected def table(schema: Option[Schema], records: Seq[Record]): NodeSeq =
    schema.fold(table(records))(table(_, records))

  protected def table(records: Seq[Record]): NodeSeq = {
    val schema = build_schema(records)
    table(schema, records)
  }

  protected def table(schema: Schema, records: Seq[Record]): NodeSeq =
    table(strategy.tableKind, schema, records)
  
  protected def table(kind: TableKind, schema: Schema, records: Seq[Record]): NodeSeq =
    table(TableOrder(Some(kind), None, Some(schema), None, None, Some(records)))

  protected def table(p: TableWithRecords): NodeSeq =
    p.kind match {
      case StandardTable => table_standard(p)
      case ListTable => table_list(p)
      case GridTable => grid(p)
      case _ => table_standard(p)
    }

  protected def table_list(p: TableWithRecords): NodeSeq = table_standard(p)

  protected def table_standard(p: TableWithRecords): NodeSeq = theme_table.container(p.table,
    <table class={theme_table.css.table(p.table)}>{
      seq(
        caption.map(x => <caption class={theme_table.css.caption(p.table)}>{nodeseq(x)}</caption>),
        Some(table_head(p.table)),
        Some(table_body(p)),
        None.map(x => <tfoot class={theme_table.css.tfoot(p.table)}></tfoot>)
      )
    }</table>
  )

  protected def table_head(tablekind: TableKind, schema: Schema): Node =
    table_head(Table(tablekind, strategy.size, schema))

  protected def table_head(p: Table): Node = p.kind match {
    case ListTable => Group(Nil)
    case _ => <thead class={theme_table.css.thead(p)}>{table_head_record(p)}</thead>
  }

  protected def table_head_record(tablekind: TableKind, schema: Schema): Node =
    table_head_record(Table(tablekind, strategy.size, schema))

  protected def table_head_record(p: Table): Elem =
    <tr class={theme_table.css.theadTr(p)}>{
      for (c <- p.schema.columns) yield <th class={theme_table.css.theadTh(p.tableColumn(c))}>{c.label(locale)}</th>
    }</tr>

  protected def table_body(kind: TableKind, schema: Schema, records: Seq[Record]): Elem =
    table_body(TableWithRecords(kind, strategy.size, schema, records))
  //   <tbody class={theme_table.css.tbody(kind)}>{table_body_records(kind, schema, records)}</tbody>

  protected def table_body(p: TableWithRecords): Elem =
    <tbody class={theme_table.css.tbody(p.table)}>{table_body_records(p)}</tbody>

  protected def table_body_records(p: TableWithRecords): Group =
    Group(p.records.toList.map(table_body_record(p.table, _)))

  // protected def table_body_records(kind: TableKind, schema: Schema, records: Seq[Record]): Group =
  //   Group(records.toList.map(table_body_record(kind, schema, _)))

  protected def table_body_record(kind: TableKind, schema: Schema, record: Record): Node =
    table_body_record(Table(kind, strategy.size, schema), record)
  //   table_record(kind, schema, record)

  protected def table_body_record(p: Table, record: Record): Node = 
    table_record(p, record)

  protected def table_body_record_data(column: Column, kind: TableKind, value: ValueModel): Elem =
    table_data(TableColumn(kind, strategy.size, column), value)

  protected def table_records(schema: Option[Schema], records: Seq[Record]): Group =
    schema.fold(
      table_records(build_schema(records), records)
    )(
      table_records(_, records)
    )

  protected def table_records(schema: Schema, records: Seq[Record]): Group =
    table_records(strategy.tableKind, schema, records)

  protected def table_records(kind: TableKind, schema: Schema, records: Seq[Record]): Group =
    table_records(TableWithRecords(kind, strategy.size, schema, records))

  protected def table_records(p: TableWithRecords): Group =
    Group(for (rec <- p.records) yield table_record(p.table, rec))

  protected def table_record(p: Table, record: Record): Node =
    p.kind match {
      case ListTable => table_record_list(p, record)
      case _ => table_record_standard(p, record)
    }

  protected def table_record(schema: Option[Schema], record: Record): Node =
    schema.fold(
      table_record(build_schema(record), record)
    )(
      table_record(_, record)
    )

  protected def table_record(schema: Schema, record: Record): Node =
    table_record(strategy.tableKind, schema, record)

  protected def table_record(kind: TableKind, schema: Schema, record: Record): Node =
    table_record(Table(kind, strategy.size, schema), record)

  protected def table_record_standard(p: Table, record: Record): Elem = {
    val attrs: Vector[(String, String)] = SeqUtils.buildTupleVector(
      Vector(
        "class" -> theme_table.css.getTbodyTr(p),
        "data-href" -> table_data_url_string(p, record)
      )
    )
    val children = for (c <- p.schema.columns) yield {
      table_data(p, c, record)
    }
    XmlUtils.element("tr", attrs, children)
  }

  protected def table_record_list(p: Table, record: Record): Node = {
    val attrs = SeqUtils.buildTupleVector(
      Vector(
        "class" -> theme_table.css.getTbodyTr(p),
        "data-href" -> table_data_url_string(p, record)
      )
    )
    val icon: Picture = picture_icon(record)
    val title = get_title(record).map(nodeseq).getOrElse(Text(""))
    val subtitle = get_subtitle(record).map(nodeseq).getOrElse(Text(""))
    val content: Node = get_content_summary(record) getOrElse Text("")
    val column = Column("dummy", XString) // TODO
    val tc = p.tableColumn(column)
    val row1 = List(
      <td class={theme_table.css.tbodyTd(tc)} rowspan="2">{table_value_img_picture{icon}}</td>,
      <td class={theme_table.css.tbodyTd(tc)}>{title}</td>
    )
    val row2 = List(
      <td class={theme_table.css.tbodyTd(tc)}>{content}</td>
    )
    Group(List(
      XmlUtils.element("tr", attrs, row1),
      XmlUtils.element("tr", attrs, row2)
    ))
  }

  protected def table_data(p: Table, column: Column, record: Record): Elem =
    table_data(p.tableColumn(column), record)

  protected def table_data(p: TableColumn, record: Record): Elem =
    <td class={theme_table.css.tbodyTd(p)}>{table_value(p, record)}</td>

  protected def table_data(p: TableColumn, v: ValueModel): Elem =
    <td class={theme_table.css.tbodyTd(p)}>{table_value(v)}</td>

  protected def table_value(column: TableColumn, record: Record): Node =
    get_table_value(column, record).getOrElse(Text(""))

  protected def table_value(v: ValueModel): Node = v match {
    case SingleValueModel(d, x) => table_value_single_option(d, x)
    case MultipleValueModel(d, xs) => RAISE.notImplementedYetDefect
  }

  protected def table_value_single_option(datatype: DataType, v: Option[Any]): Node =
    v.map(table_value_single(datatype, _)).getOrElse(Text(""))

  protected def table_value_single(datatype: DataType, v: Any): Node =
    datatype match {
      case XDateTime => table_value_datetime(v)
      case XDate => table_value_date(v)
      case XTime => table_value_time(v)
      case XEverforthid => table_value_everforthid(v)
      case XLink => table_value_link(v)
      case XImageLink => table_value_img(v)
      case XHtml => table_value_html(v)
      case _ => table_value_string(v)
    }

  protected def get_table_value(column: TableColumn, record: Record): Option[Node] = {
    // TODO datetime formatting
    val c = column.column
    c.datatype match {
      case XDateTime => table_get_value_datetime(c, record)
      case XDate => table_get_value_date(c, record)
      case XTime => table_get_value_time(c, record)
      case XEverforthid => table_get_value_everforthid(c, record)
      case XLink => table_get_value_link(c, record)
      case XImageLink => table_get_value_img(column, record)
      case XHtml => table_get_value_html(c, record)
      case XText => table_get_value_text(c, record)
      case _ => table_get_value_string(c, record)
    }
  }

  protected def table_get_value_datetime(column: Column, record: Record): Option[Node] =
    record.getFormTimestamp(column.name).map(table_value_datetime)

  protected def table_get_value_date(column: Column, record: Record): Option[Node] =
    record.getFormDate(column.name).map(table_value_date)
//     record.getFormDate(column.name) flatMap {
//       case m: java.util.Date => Some(Text(DateUtils.toIsoDateString(m)))
// //      case m: DateTime => DateUtils.toIsoDateString(m)
// //      case m: LocalDate => DateUtils.toIsoDateString(m)
//       case m => table_get_value_string(column, record)
//     }

  protected def table_get_value_time(column: Column, record: Record): Option[Node] =
    table_get_value_string(column, record) // XXX

  protected def table_get_value_everforthid(column: Column, record: Record): Option[Node] =
    record.getString(column.name).map(table_value_everforthid)

  protected def table_get_value_link(column: Column, record: Record): Option[Node] =
    record.getFormOne(column.name).map(table_value_link)

  protected def table_get_value_image_link(column: Column, record: Record): Option[Node] =
    record.getFormOne(column.name).map(table_value_image_link)

  protected def table_get_value_img(column: TableColumn, record: Record): Option[Node] =
    record.getFormOne(column.name).map(table_value_img(column, _))

  protected def table_get_value_html(column: Column, record: Record): Option[Node] =
    record.getFormString(column.name).map(table_value_html)

  protected def table_get_value_text(column: Column, record: Record): Option[Node] =
    record.getFormString(column.name).map(table_value_text)

  protected def table_get_value_string(column: Column, record: Record): Option[Node] =
    record.getString(column.name).map(Text(_))

  protected def table_value_datetime(x: Any): Node = {
    def print(label: String, jst: String, gmt: String) =
      <span data-toggle="tooltip" title={s"$jst(JST) $gmt(GMT)"}>{label}</span>
    x match {
      case m: java.sql.Timestamp =>
        print(
          //        DateTimeUtils.toSimpleString24Jst(x),
          DateTimeUtils.toSimpleStringJst(m),
          DateTimeUtils.toSimpleStringJst(m),
          DateTimeUtils.toSimpleStringGmt(m)
        )
    }
  }

  protected def table_value_date(x: Any): Node = x match {
    case m: java.util.Date => Text(DateUtils.toIsoDateString(m))
  }

  protected def table_value_time(x: Any): Node =
    table_value_string(x) // TODO

  protected def table_value_everforthid(x: Any): Node = {
    val id = x.toString
    val s = id.takeRight(3)
    //      <a href={id} data-toggle="tooltip" title={id} data-placement="auto right">{s}</a>
    val text = s"""<a href="$id">$id</a>"""
    <span data-toggle="popover" data-content={text} data-html="true">{s}</span>
  }

  protected def table_value_link(p: Any): Node = {
    val x = p.toString
    val s = try {
      val a = make_html_uri(x)
      StringUtils.pathLastComponentBody(a.getPath())
    } catch {
      case NonFatal(e) => x
    }
    <span data-toggle="tooltip" title={x}>{s}</span>
  }

  protected def table_value_image_link(p: Any): Node = p match {
    case m: String => table_value_image_link_string(m)
    case m: URI => table_value_image_link_uri(m)
    case m: URL => table_value_image_link_url(m)
    case m: Picture => table_value_image_link_picture(m)
    case m => table_value_image_link_string(m.toString)
  }

  protected def table_value_image_link_string(p: String): Node =
    table_value_image_link_uri(new URI(p))

  protected def table_value_image_link_url(p: URL): Node =
    table_value_image_link_uri(p.toURI)

  protected def table_value_image_link_uri(p: URI): Node = {
    val s = StringUtils.pathLastComponentBody(p.getPath())
    <span data-toggle="tooltip" title={p.toString}>{s}</span>
  }

  protected def table_value_image_link_picture(p: Picture): Node = {
    val src = p.src.toString
    val a = p.alt.map(string).getOrElse(src)
    val s = StringUtils.pathLastComponentBody(a)
    <span data-toggle="tooltip" title={src}>{s}</span>
  }

  protected def table_value_img(column: TableColumn, p: Any): Node = p match {
    case m: String => table_value_img_string(column, m)
    case m: URI => table_value_img_uri(column, m)
    case m: URL => table_value_img_url(column, m)
    case m: Picture => table_value_img_picture(column, m)
    case m => table_value_img_string(column, m.toString)
  }

  protected def table_value_img_string(column: TableColumn, p: String): Node =
    table_value_img_uri(new URI(p))

  protected def table_value_img_url(column: TableColumn, p: URL): Node =
    table_value_img_uri(p.toURI)

  protected def table_value_img_uri(column: TableColumn, p: URI): Node =
    <img src={p.toString}></img>

  protected def table_value_img_picture(column: TableColumn, p: Picture): Node = {
    val alt: String = p.alt.map(string).getOrElse("")
    val src = column.kind match {
      case PropertyTable => p.l
      case _ => p.xs
    }
    <img class={theme_table.css.img(strategy.tableKind)} src={src} alt={alt}></img>
  }

  protected def table_value_img(p: Any): Node = p match {
    case m: String => table_value_img_string(m)
    case m: URI => table_value_img_uri(m)
    case m: URL => table_value_img_url(m)
    case m: Picture => table_value_img_picture(m)
    case m => table_value_img_string(m.toString)
  }

  protected def table_value_img_string(p: String): Node =
    table_value_img_uri(new URI(p))

  protected def table_value_img_url(p: URL): Node =
    table_value_img_uri(p.toURI)

  protected def table_value_img_uri(p: URI): Node =
    <img src={p.toString}></img>

  protected def table_value_img_picture(p: Picture): Node = {
    val alt: String = p.alt.map(string).getOrElse("")
    val src = strategy.tableKind match {
      case PropertyTable => p.l
      case _ => p.xs
    }
    <img class={theme_table.css.img(strategy.tableKind)} src={src} alt={alt}></img>
  }

  protected def table_value_html(p: Any): Node = p match {
    case m: String => DomUtils.toXml(DomUtils.parseHtmlFragment(m))
    case m: Node => m
    case m: org.w3c.dom.Node => DomUtils.toXml(m)
  }

  // TODO summarize
  protected def table_value_html_summary(p: Any): Node = p match {
    case m: String => DomUtils.toXml(DomUtils.parseHtmlFragment(m))
    case m: Node => m
    case m: org.w3c.dom.Node => DomUtils.toXml(m)
  }

  protected def table_value_text(p: Any): Node = <pre>{p.toString}</pre>

  protected def table_value_string(x: Any): Node = Text(x.toString)

  protected def property_table(schema: Option[Schema], records: Seq[Record]): NodeSeq =
    table(PropertyTable, schema, records)

  protected def property_table(schema: Schema, records: Seq[Record]): NodeSeq =
    table(PropertyTable, schema, records)

  protected def entity_property_sheet(
    entitytype: DomainEntityType,
    schema: Option[Schema],
    record: Record
  ): NodeSeq = schema.fold(
    property_sheet(schema, record)
  )(s =>
    property_sheet(
      strategy.withUsageKind(DetailUsage).resolveSchema(entitytype, s),
      record
    )
  )

  protected def property_sheet(schema: Option[Schema], record: Record): NodeSeq =
    schema.fold(property_sheet(record))(property_sheet(_, record))

  protected def property_sheet(record: Record): NodeSeq = {
    val schema = build_schema(Vector(record))
    property_sheet(schema, record)
  }

  protected def property_sheet(schema: Schema, record: Record): NodeSeq = {
    val t = Table(PropertyTable, strategy.size, schema)
    <table class={theme_table.css.table(t)}>
      <tbody>{
        for (c <- schema.columns) yield {
          <tr class={theme_table.css.tbodyTr(t)}>{
            val tc = t.tableColumn(c)
            List(
              <th class={theme_table.css.theadTh(tc)}>{c.label(locale)}</th>,
              table_data(tc, record)
            )
          }</tr>
        }
      }</tbody>
    </table>
  }

  protected def property_sheet_confirm(): NodeSeq = RAISE.notImplementedYetDefect

  protected def property_input_form(
    action: URI,
    method: Method,
    schema: Schema,
    record: Record,
    hidden: Hidden,
    submits: Submits
  ): NodeSeq =
    strategy.theme match {
      case m: Bootstrap4RenderThemaBase => 
        property_input_form_bootstrap(
          InputForm(action, method, schema, record, hidden, submits))
      case m =>
        property_input_form_table(action, method, schema, record, hidden, submits)
    }

  protected def property_input_form_bootstrap(p: InputForm): NodeSeq = {
    <div class="container">
    <form method="GET" action="">{
      val a = for (c <- p.schema.columns) yield property_input_form_item_bootstrap(p, c)
      a ++ List(
        <div class="form-group row justify-content-center">{
          for (s <- p.submits.submits) yield {
            <div class="col-2">
            <button type="submit" class="btn btn-default btn-lg btn-block" name={s.name} value={s.value(strategy.locale)}>{s.value(strategy.locale)}</button>
            </div>
          }
        }</div>
      )
    }</form>
    </div>
  }

  protected def property_input_form_item_bootstrap(p: InputForm, c: Column): Elem =
    c.datatype match {
      case XDateTime => _property_input_form_item_datetime_bootstrap(p, c)
      case m: XPowertype => _property_input_form_item_powertype_bootstrap(p, c, m)
      case XImageLink => _property_input_form_item_imagelink_bootstrap(p, c)
      case XFile => _property_input_form_item_file_bootstrap(p, c)
      case _ => _property_input_form_item_bootstrap(p, c)
    }

  private def _property_input_form_item_datetime_bootstrap(p: InputForm, c: Column): Elem = {
    val id = generate_id()
    val idproperty = generate_id()
    val property = c.name
    add_javascript_in_footer("""
        $('#%s').datetimepicker({
          autoclose: true,
          todayBtn: true,
          language: 'ja'
        });
      """.format(idproperty)
    )
    <div class="form-group row">
    <label class="col-form-label col-sm-2" for={id}>{c.label(locale)}:</label>
    <div class="input-group date col-sm-5" id={idproperty}>{
      List(
        XmlUtils.appendAttributes(
          <input type="text" name={property} class="form-control" id={id} />,
          "placeholder" -> None
        ),
        <span class="input-group-addon">
          <i class="fa fa-times"/>
          <i class="fa fa-calendar"/>
        </span>
      )
    }</div>
    </div>
  }

  private def _property_input_form_item_powertype_bootstrap(p: InputForm, c: Column, pt: XPowertype): Elem = {
    val id = generate_id()
    <div class="form-group row">
    <label class="control-label col-sm-2" for={id}>{c.label(locale)}:</label>
    <div class="col-sm-10">
    <select class="custom-select" id={id}>{
      List(
        c.form.placeholder.map(x =>
          <option selected="">{x}</option>
        ).getOrElse(Group(Nil)),
        for (t <- pt.powertype.elements) yield {
          <option value={t.name}>t.label</option>
        }
      )
    }
    </select>
    </div>
    </div>
  }

  private def _property_input_form_item_imagelink_bootstrap(p: InputForm, c: Column): Elem = {
    val id = generate_id()
    <div class="form-group row">
    <label class="control-label col-sm-2" for={id}>{c.label(locale)}:</label>
    <div class="col-sm-10">
      <input type="file" class="form-control" id={id} name={c.name} />
    </div>
    </div>
  }

  private def _property_input_form_item_file_bootstrap(p: InputForm, c: Column): Elem = {
    val id = generate_id()
    <div class="form-group row">
    <label class="control-label col-sm-2" for={id}>{c.label(locale)}:</label>
    <div class="col-sm-10">
      <input type="file" class="form-control" id={id} name={c.name} />
    </div>
    </div>
  }

  private def _property_input_form_item_bootstrap(p: InputForm, c: Column): Elem = {
    val id = generate_id()
    <div class="form-group row">
    <label class="control-label col-sm-2" for={id}>{c.label(locale)}:</label>
    <div class="col-sm-10">{
      XmlUtils.appendAttributes(
        <input type={_form_type(c)} class="form-control" id={id} name={c.name} />,
        "placeholder" -> c.form.placeholder.map(_(locale))
      )
    }</div>
    </div>
  }

  protected def property_input_form_table(
    action: URI,
    method: Method,
    schema: Schema,
    record: Record,
    hidden: Hidden,
    submits: Submits
  ): NodeSeq = {
    val t = Table(FormTable, strategy.size, schema)
    <form action={action.toString} method={method.name}>
      <table class={theme_table.css.table(t)}>
      <tbody>{
        for (c <- schema.columns) yield {
          val tc = t.tableColumn(c)
          <tr class={theme_table.css.tbodyTr(t)}>{
            List(
              <th class={theme_table.css.theadTh(tc)}>{c.label(locale)}</th>,
              input_field(tc, record)
            )
          }</tr>
        }
      }</tbody>
      </table>
      <table>
        <tr>{
          for (s <- submits.submits) yield {
            <td><input type="submit" name={s.name} value={s.value(strategy.locale)}></input></td>
          }
        }</tr>
      </table>
      {hidden.render}
    </form>
  }

  protected def input_field(tc: TableColumn, record: Record) = {
    val column = tc.column
    if (column.form.readonly)
      table_data(tc, record)
    else
      <td class={theme_table.css.tbodyTd(tc)}>{_input_field(column, record)}</td>
  }

  protected def _input_field(column: Column, record: Record) = {
    val s = record.getString(column.name) getOrElse ""
    column.datatype match {
      case XText => <textarea name={column.name} rows="4" value={s}></textarea>
      case _ => <input type="text" name={column.name} value={s}></input>
    }
  }

  protected def get_hidden_data(column: Column, record: Record) =
    record.getString(column.name).map(s =>
      <input type="hidden" name={column.name} value={s}></input>
    )

  protected def property_confirm_form(
    action: URI,
    method: Method,
    schema: Schema,
    record: Record,
    hidden: Hidden,
    submits: Submits
  ): NodeSeq = {
    val t = Table(FormTable, strategy.size, schema)
    <form action={action.toString} method={method.name}>
      <table class={theme_table.css.table(t)}>
      <tbody>{
        for (c <- schema.columns) yield {
          val tc = t.tableColumn(c)
          <tr class={theme_table.css.tbodyTr(t)}>{
            List(
              <th class={theme_table.css.theadTh(tc)}>{c.label(locale)}</th>,
              table_data(tc, record)
            )
          }</tr>
        }
      }</tbody>
      </table>
      {
        schema.columns.flatMap(get_hidden_data(_, record))
      }
      <table>
        <tr>{
          for (s <- submits.submits) yield {
            <td><input type="submit" name={s.name} value={s.value(strategy.locale)}></input></td>
          }
        }</tr>
      </table>
      {hidden.render}
    </form>
  }

  // Bootstrap4
  protected def searchbox_form(p: SearchBox): Elem = {
    <div class="container">
    <form method="GET">{
      val a = for (c <- p.schema.columns) yield searchbox_form_item(p, c)
      a ++ List(
        <div class="form-group row justify-content-center">
        <div class="col">
        <button type="submit" class="btn btn-default btn-lg btn-block">{button_search}</button>
        </div>
        </div>
      )
    }</form>
    </div>
  }

  protected def searchbox_form_item(p: SearchBox, c: Column): Elem =
    c.datatype match {
      case XDateTime => _searchbox_form_item_datetime(p, c)
      case m: XPowertype => _searchbox_form_item_powertype(p, c, m)
      case _ => _searchbox_form_item(p, c)
    }

  private def _searchbox_form_item_datetime(p: SearchBox, c: Column): Elem = {
    val id = generate_id()
    val idstart = generate_id()
    val startproperty = s"${c.name}.start"
    val idend = generate_id()
    val endproperty = s"${c.name}.end"
    add_javascript_in_footer("""
        $('#%s').datetimepicker({
          autoclose: true,
          todayBtn: true,
          language: 'ja'
        });
        $('#%s').datetimepicker({
          autoclose: true,
          todayBtn: true,
          language: 'ja'
        });
      """.format(idstart, idend)
    )
    <div class="form-group row">
    <label class="col-form-label col-sm-2" for={id}>{c.label(locale)}:</label>
    <div class="input-group date col-sm-5" id={idstart}>{
      List(
        XmlUtils.appendAttributes(
          <input type="text" name={startproperty} class="form-control" id={id} />,
          "placeholder" -> Some(placeholder_start)
        ),
        <span class="input-group-addon">
          <i class="fa fa-times"/>
          <i class="fa fa-calendar"/>
        </span>
      )
    }</div>
    <div class="input-group date col-sm-5" id={idend}>{
      List(
        XmlUtils.appendAttributes(
          <input type="text" name={endproperty} class="form-control" id={id} />,
          "placeholder" -> Some(placeholder_end)
        ),
        <span class="input-group-addon">
          <i class="fa fa-times"/>
          <i class="fa fa-calendar"/>
        </span>
      )
    }</div>
    </div>
  }

  private def _searchbox_form_item_powertype(p: SearchBox, c: Column, pt: XPowertype): Elem = {
    val id = generate_id()
    <div class="form-group row">
    <label class="control-label col-sm-2" for={id}>{c.label(locale)}:</label>
    <div class="col-sm-10">
    <select class="custom-select" id={id}>{
      List(
        c.form.placeholder.map(x =>
          <option selected="">{x}</option>
        ).getOrElse(Group(Nil)),
        for (t <- pt.powertype.elements) yield {
          <option value={t.name}>t.label</option>
        }
      )
    }
    </select>
    </div>
    </div>
  }

  private def _searchbox_form_item(p: SearchBox, c: Column): Elem = {
    val id = generate_id()
    <div class="form-group row">
    <label class="control-label col-sm-2" for={id}>{c.label(locale)}:</label>
    <div class="col-sm-10">{
      XmlUtils.appendAttributes(
        <input type={_form_type(c)} class="form-control" id={id} name={c.name} />,
        "placeholder" -> c.form.placeholder.map(_(locale))
      )
    }</div>
    </div>
  }

  private def _form_type(c: Column): String = _form_type(c.datatype)
  // control type: button, hidden, reset, submit
  private def _form_type(p: DataType): String =
    p.getHtmlInputTypeName getOrElse "text"

  protected def grid(p: TableWithRecords): Elem = {
    val cards = p.records.map(to_card(p.table, _)).toList
    grid(cards)
    // val grid = strategy.gridContext
    // val ncolumns = 6
    // val width = 12 / ncolumns
    // <div class="container"> { // container-fluid
    //   for (row <- p.records.grouped(ncolumns)) yield {
    //     <div class="row"> {
    //       for (x <- row) yield {
    //         <div class={s"col-sm-${width}"}> {
    //           card(x)
    //         } </div>
    //       }
    //     } </div>
    //   }
    // } </div>
  }

  protected def grid(ps: List[Card]): Elem =
    if (ps.length == 0)
      empty_block
    else
      grid_with_content(ps)(card(_))

  protected def grid_with_content[T](ps: List[T])(f: T => NodeSeq): Elem =
    strategy.theme match {
      case m if (m.isGridDiv) => grid_div(ps)(f)
      case m if (m.isGridTable) => grid_table(ps)(f)
      case m: Bootstrap4RenderThemaBase => grid_bootstrap4(ps)(f)
      case m: Bootstrap3RenderThemaBase => grid_bootstrap3(ps)(f)
      case m => grid_table(ps)(f)
    }

  protected def grid_with_content_bootstrap[T](ps: List[T])(f: T => NodeSeq): Elem =
    strategy.theme match {
      case m: Bootstrap4RenderThemaBase => grid_bootstrap4(ps)(f)
      case m: Bootstrap3RenderThemaBase => grid_bootstrap3(ps)(f)
      case m => grid_table(ps)(f)
    }

  protected def grid_bootstrap4[T](ps: List[T])(f: T => NodeSeq): Elem = {
    val g = strategy.gridContext
    val w = g.width
    val xscolumns = g.ncolumns.get(PhoneScreen) // < 576px
    val smcolumns = g.ncolumns.get(PhabletScreen) // >= 576px
    val mdcolumns = g.ncolumns.get(TabletScreen) // >= 768px
    val lgcolumns = g.ncolumns.get(LaptopScreen) // >= 992px
    val xlcolumns = g.ncolumns.get(DesktopScreen) // >= 1200px
    val colclass = Vector(
      xscolumns.map(x => s"col-${w / x}"),
      smcolumns.map(x => s"col-sm-${w / x}"),
      mdcolumns.map(x => s"col-md-${w / x}"),
      lgcolumns.map(x => s"col-lg-${w / x}"),
      xlcolumns.map(x => s"col-xl-${w / x}")
    ).flatten.mkString(" ")
    <div class="container"> { // container-fluid
      for (row <- ps.grouped(w)) yield {
        val rowclass = if (g.isNoGutters)
          "row no-gutters"
        else
          "row"
        <div class={rowclass}> {
          for (x <- row) yield {
            <div class={colclass}> {
              f(x)
            } </div>
          }
        } </div>
      }
    } </div>
  }

  protected def grid_bootstrap3[T](ps: List[T])(f: T => NodeSeq): Elem = {
    val g = strategy.gridContext
    val w = g.width
    val xscolumns = g.ncolumns.get(PhoneScreen) // < 768px
    val smcolumns = g.ncolumns.get(PhabletScreen) // >= 768px
    val mdcolumns = g.ncolumns.get(TabletScreen) // >= 992px
    val lgcolumns = g.ncolumns.get(LaptopScreen) orElse g.ncolumns.get(DesktopScreen) // >= 1200px
    val colclass = Vector(
      xscolumns.map(x => s"col-xs-${w / x}"),
      smcolumns.map(x => s"col-sm-${w / x}"),
      mdcolumns.map(x => s"col-md-${w / x}"),
      lgcolumns.map(x => s"col-lg-${w / x}")
    ).flatten.mkString(" ")
    <div class="container"> { // container-fluid
      for (row <- ps.grouped(w)) yield {
        val rowclass = if (g.isNoGutters)
          "row no-gutters"
        else
          "row"
        <div class={rowclass}> {
          for (x <- row) yield {
            <div class={colclass}> {
              f(x)
            } </div>
          }
        } </div>
      }
    } </div>
  }

  protected def grid_table[T](ps: List[T])(f: T => NodeSeq): Elem = {
    val g = strategy.gridContext
    val ncolumns = g.ncolumns.get(strategy.renderContext.screenKind) getOrElse {
      g.defaultNColumns
    }
    <div class={theme_grid.css.table.container}>{
      <table class={theme_grid.css.table.table}>
        <tbody class={theme_grid.css.table.tbody}>{
          for (row <- ps.grouped(ncolumns)) yield {
            <tr class={theme_grid.css.table.tbodyTr}>{
              for (x <- row) yield f(x)
            }</tr>
          }
        }</tbody>
      </table>
    }</div>
  }

  protected def grid_div[T](ps: List[T])(f: T => NodeSeq): Elem = {
    val g = strategy.gridContext
    val ncolumns = g.ncolumns.get(strategy.renderContext.screenKind) getOrElse {
      g.defaultNColumns
    }
    val r = <div class={theme_grid.css.div.container}>{
      <div class={theme_grid.css.div.row}>{
        for (row <- ps.grouped(ncolumns)) yield {
          <div class={theme_grid.css.div.field}>{
            for (x <- row) yield f(x)
          }</div>
        }
      }</div>
    }</div>
    XmlUtils.adjustEmptyDiv(r)
  }

  protected def to_card(rec: Record): Card = {
    val icon = picture_icon(rec)
    val title = get_title(rec)
    val subtitle = get_subtitle(rec)
    val header = TitleLine(title, subtitle).toOption
    val content = get_content(rec)
    val link = get_link(rec)
    Card.create(icon, header, content, link, rec)
  }

  protected def to_card(table: Table, rec: Record): Card =
    strategy.theme match {
      case MyColorTheme => _to_card_productclass_mycolor(table, rec)
      case _ => _to_card(table, rec)
    }

  private def _to_card_productclass_mycolor(table: Table, rec: Record): Card = {
    val icon = picture_icon(rec)
    val title = get_title(rec).map(_(locale)) getOrElse Text("")
    val price = Text("10,000") // TODO
    val content =
      <div class="c-card__textArea">
        <div class="c-card__textArea__name">{title}</div>
        <div class="c-card__textArea__price">{title}</div>
      </div>
    val link = get_link(table, rec)
    Card.create(icon, None, Some(content), link, rec)
  }

  private def _to_card(table: Table, rec: Record): Card = {
    val icon = picture_icon(rec)
    val title = get_title(rec)
    val subtitle = get_subtitle(rec)
    val header = TitleLine(title, subtitle).toOption
    val content = get_content(rec)
    val link = get_link(table, rec)
    Card.create(icon, header, content, link, rec)
  }

  protected def card(rec: Record): NodeSeq = card(to_card(rec))

  // TableCardModel 
  protected def card(
    imagetop: Option[Picture],
    header: Option[TitleLine],
    footer: Option[TitleLine],
    content: NodeSeq
  ): NodeSeq = card(Card.create(imagetop, header, footer, content))

  protected def card(p: Card): NodeSeq = {
    val card = {
      val imagetop: Option[Picture] =
        if (strategy.cardKind.isImageTop)
          p.image_top // TODO default
        else
          None
      val header: Option[TitleLine] =
        if (strategy.cardKind.isHeader)
          p.header orElse Some(TitleLine.blank)
        else
          None
      val footer: Option[TitleLine] =
        if (strategy.cardKind.isFooter)
          p.footer orElse Some(TitleLine.blank)
        else
          None
      val content: Option[I18NElement] =
        if (strategy.cardKind.isContent)
          p.content orElse Some(I18NElement(""))
        else
          None
      Card(imagetop, header, footer, content, p.link, p.record)
    }
    card_component(card) getOrElse {
      strategy.theme match {
        case m if m.isCardDiv => card_div(card)
        case m if m.isCardTable => card_table(card)
        case m: Bootstrap4RenderThemaBase => card_bootstrap(card)
        case m: Bootstrap3RenderThemaBase => card_paperdashboard(card)
        case m => card_table(card)
      }
    }
  }

  protected def card_component(card: Card): Option[NodeSeq] =
    strategy.renderContext.cardKind flatMap {
      case m: ComponentCard =>
        strategy.viewContext.flatMap { x =>
          val a = x.parcel.withModel(CardModel(card, s"card__${m.name}"))
          strategy.viewContext.flatMap(_.engine.renderComponentOption(a))
        }
      case _ => None
    }

  // Bootstrap 4
  protected def card_bootstrap(card: Card): Elem = {
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
    def body: NodeSeq = Group(
      card.image_top.map(i =>
        i.href.fold {
          img(i)
        } { href =>
          <a href={href.toString} target="_blank">{img(i)}</a>
        }
      ).toVector ++ card.header.map(h =>
        <div class="card-header">{
          card_title_bootstrap(h)
        }</div>
      ).toVector ++ card.content.map(c => <div class="card-block">{
        <div class="card-text">{c}</div>
      }</div>).toVector ++ card.footer.map(f =>
        <div class="card-footer">{
          card_title_bootstrap(f)
        }</div>
      ).toVector
    )
    def bodywithlink: NodeSeq =
      card.link.fold(body) { link =>
        link.getDataHref(render_context).fold(body) { href =>
          <a class={theme_card.css.table.anchor} href={href.toString}>{body}</a>
        }
      }
    <div class="card"> {
      bodywithlink
    } </div>
  }

  def card_title_bootstrap(p: TitleLine): List[Node] = {
    List(
      p.title.map(t => <h4 class="card-title">{t(locale)}</h4>),
      p.subtitle.map(d => <p class="card-subtitle">{d(locale)}</p>)
    ).flatten
  }

  protected def card_paperdashboard(card: Card): Elem = {
    <div class="card">{
      card.image_top.map(picture(_)) ++ card.header.map(h =>
        <div class="header">{
          List(
            h.title.map(t => <h4 class="title">{t(locale)}</h4>),
            h.subtitle.map(d => <p class="category">{d(locale)}</p>)
          ).flatten
        }</div>
      ) ++ card.content.map(c =>
        <div class="content">{
          c(locale) ++ List(
            card.footer.map(f =>
              <div class="footer">{
                List(
                  f.title.map(t => <h4 class="title">{t(locale)}</h4>),
                  f.subtitle.map(d => <p class="category">{d(locale)}</p>)
                ).flatten
              }</div>
            )
          ).flatten
        }</div>
      )
    }</div>
  }

  protected def card_table(card: Card): Elem = {
    def render = {
      def tl(p: TitleLine): Vector[Node] = Vector(
        p.title.map(x => <h3>{x(locale)}</h3>),
        p.subtitle.map(x => <p>{x(locale)}</p>)
      ).flatten
      <table> {
        Vector(
          card.image_top.toVector.flatMap(picture).map(x => <tr><td>{x}</td></tr>), // XXX css
          card.header.toVector.flatMap(tl).map(x => <tr><td>{x}</td></tr>), // XXX css
          card.content.toVector.flatMap(_(locale)).map(x => <tr><td>{x}</td></tr>), // XXX css
          card.footer.toVector.flatMap(tl).map(x => <tr><td>{x}</td></tr>)
        ).flatten // XXX css
      }</table>
    }
    def data: NodeSeq = card.image_top.fold(render)(it =>
      if (card.isImageTopOnly)
        picture(it)
      else
        render
    )
    def datawithlink: NodeSeq = card.link.fold(data) { link =>
      link.getDataHref(render_context).fold(data) { href =>
        <a class={theme_card.css.table.anchor} href={href.toString}>{data}</a>
      }
    }
    <td class={theme_card.css.table.td}>{datawithlink}</td>
  }

  protected def card_div(card: Card): Elem = {
    def f(p: Picture): Elem = { // MyColor
      <div class="c-card__imageArea">
        <img class="c-card__imageArea__image" src={p.src.toString} alt={p.alt.map(node)} />
      </div>
    }
    def render = {
      def tl(p: TitleLine): Vector[Node] = Vector(
        p.title.map(x => <h3>{x(locale)}</h3>),
        p.subtitle.map(x => <p>{x(locale)}</p>)
      ).flatten
      <div class={theme_card.css.div.card}> {
        Vector(
          card.image_top.toVector.flatMap(f).map(x => <div class={theme_card.css.div.imageTop}>{x}</div>),
          card.header.toVector.flatMap(tl).map(x => <div class={theme_card.css.div.header}>{x}</div>),
          card.content.toVector.flatMap(_(locale)).map(x => <div class={theme_card.css.div.content}>{x}</div>),
          card.footer.toVector.flatMap(tl).map(x => <div class={theme_card.css.div.footer}>{x}</div>)
        ).flatten
      }</div>
    }
    def data: NodeSeq = card.image_top.fold(render)(it =>
      if (card.isImageTopOnly)
        picture(it)
      else
        render
    )
    def datawithlink: NodeSeq = card.link.fold(data) { link =>
      link.getDataHref(render_context).fold(data) { href =>
        <a class={theme_card.css.div.anchor} href={href.toString}>{data}</a>
      }
    }
    <div class={theme_card.css.div.container}>{datawithlink}</div>
  }

  protected def get_title(rec: Record): Option[I18NElement] =
    rec.getString(KEY_DOMAIN_OBJECT_TITLE).map(I18NElement.parse)

  protected def get_subtitle(rec: Record): Option[I18NElement] =
    rec.getString(KEY_DOMAIN_OBJECT_SUBTITLE).map(I18NElement.parse)

  protected def get_content(rec: Record): Option[Node] =
    rec.getString(KEY_DOMAIN_OBJECT_CONTENT).map(table_value_html)

  protected def get_content_summary(rec: Record): Option[Node] =
    rec.getString(KEY_DOMAIN_OBJECT_CONTENT).map(table_value_html_summary)

  protected def picture_icon(rec: Record): Picture =
    rec.getOne(KEY_DOMAIN_OBJECT_IMAGE_ICON).fold {
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
    val s: RenderStrategy = strategy.withCardKind(ImageCard).withGridContext(g)
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

  /*
   * Utilities
   */
  protected final def build_schema(record: Record): Schema =
    RecordUtils.buildSchema(Vector(record))

  protected final def build_schema(rs: Seq[Record]): Schema =
    RecordUtils.buildSchema(rs)
}

object Renderer {
  case class TableOrder(
    kind: Option[TableKind],
    size: Option[RenderSize],
    schema: Option[Schema],
    entityType: Option[DomainEntityType],
    dataHref: Option[URI],
    records: Option[Seq[Record]]
  )
  object TableOrder {
    def apply(
      kind: TableKind,
      schema: Option[Schema],
      entitytype: DomainEntityType,
      records: Seq[Record]
    ): TableOrder = TableOrder(Some(kind), None, schema, Some(entitytype), None, Some(records))

    def apply(
      kind: TableKind,
      schema: Option[Schema],
      entitytype: Option[DomainEntityType],
      records: Seq[Record]
    ): TableOrder = TableOrder(Some(kind), None, schema, entitytype, None, Some(records))

    def apply(
      kind: Option[TableKind],
      schema: Option[Schema],
      entitytype: DomainEntityType,
      records: Seq[Record]
    ): TableOrder = TableOrder(kind, None, schema, Some(entitytype), None, Some(records))

    def apply(
      kind: Option[TableKind],
      schema: Option[Schema],
      entitytype: Option[DomainEntityType],
      datahref: Option[URI],
      records: Seq[Record]
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
    records: Seq[Record]
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
      records: Seq[Record]
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
      records: Seq[Record]
    ): TableWithRecords = TableWithRecords(
      Table(kind, size, schema, entitytype, datahref),
      records
    )

    def apply(
      kind: TableKind,
      size: RenderSize,
      schema: Schema,
      records: Seq[Record]
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
    record: Record
  )

  case class InputForm(
    action: URI,
    method: Method,
    schema: Schema,
    record: Record,
    hidden: Hidden,
    submits: Submits
  )

  case class SearchBox(
    schema: Schema
  )
}
