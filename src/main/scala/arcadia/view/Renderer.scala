package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import scala.util.control.NonFatal
import java.util.Locale
import java.net.{URI, URL}
import org.joda.time.DateTime
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.record.v2.util.RecordUtils
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.xml.XmlUtils
import org.goldenport.xml.dom.DomUtils
import org.goldenport.util.{DateTimeUtils, DateUtils, StringUtils, SeqUtils}
import arcadia.model._
import arcadia.domain._

/*
 * @since   Aug.  1, 2017
 *  version Aug. 29, 2017
 *  version Sep. 26, 2017
 * @version Oct. 18, 2017
 * @author  ASAMI, Tomoharu
 */
abstract class Renderer(
  strategy: RenderStrategy,
  pageName: Option[I18NString],
  headline: Option[I18NElement],
  title: Option[I18NElement],
  caption: Option[I18NElement]
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

  def apply: NodeSeq = strategy.scope match {
    case Html => render_html
    case Body => render_body
    case Section => render_section
    case Content => render_content
  }

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
    backUri: Option[URI]
  ): NodeSeq = <div>ERROR: {code}</div> // TODO

  protected def text(p: String): Text = Text(p)

  protected def seq(head: Option[NodeSeq], tail: Option[NodeSeq]*): Seq[NodeSeq] =
    (head :: tail.toList).flatten

  protected def h2(p: Option[I18NString]): Node = p.map(x => <h2>string(x)</h2>).getOrElse(Group(Nil))

  protected def tabular(schema: Option[Schema], records: Seq[Record]): NodeSeq =
    schema.fold(tabular(records))(tabular(_, records))

  protected def tabular(records: Seq[Record]): NodeSeq = {
    val schema = build_schema(records)
    tabular(schema, records)
  }

  protected def tabular(schema: Schema, records: Seq[Record]): NodeSeq = <table class={theme_table.className.table(TabularTable)}>{
    seq(
      caption.map(x => <caption class={theme_table.className.tbody(TabularTable)}>{nodeseq(x)}</caption>),
      Some(table_records(TabularTable, schema, records))
    )
  }</table>

  protected def table(p: TableOrder): NodeSeq = {
    val records = p.records getOrElse Nil
    val kind = p.kind getOrElse strategy.tableKind
    val schema = strategy.resolveSchema(p)
//    val schema = p.schema getOrElse build_schema(records)
    val entitytype = p.entityType
    val t = Table(kind, schema, entitytype, records)
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
    table(TableOrder(Some(kind), Some(schema), None, Some(records)))

  protected def table(p: Table): NodeSeq =
    p.kind match {
      case StandardTable => table_standard(p)
      case ListTable => table_list(p)
      case GridTable => grid(p)
      case _ => table_standard(p)
    }

  protected def table_list(p: Table): NodeSeq = table_standard(p)

  protected def table_standard(p: Table): NodeSeq = theme_table.container(p.kind, p.schema, p.records,
    <table class={theme_table.className.table(p.kind)}>{
      seq(
        caption.map(x => <caption class={theme_table.className.caption(p.kind)}>{nodeseq(x)}</caption>),
        Some(table_head(p)),
        Some(table_body(p)),
        None.map(x => <tfoot class={theme_table.className.tfoot(p.kind)}></tfoot>)
      )
    }</table>
  )

  protected def table_head(p: Table): Node = p.kind match {
    case ListTable => Group(Nil)
    case _ => table_head(p.kind, p.schema)
  }

  protected def table_head(kind: TableKind, schema: Schema): Elem =
    <thead class={theme_table.className.thead(kind)}>{table_head_record(kind, schema)}</thead>

  protected def table_head_record(kind: TableKind, schema: Schema): Elem =
    <tr classw={theme_table.className.theadTr(kind)}>{
      for (c <- schema.columns) yield <th class={theme_table.className.theadTh(kind)}>{c.label(locale)}</th>
    }</tr>

  protected def table_body(p: Table): Elem =
    <tbody class={theme_table.className.tbody(p.kind)}>{table_body_records(p)}</tbody>

  protected def table_body(kind: TableKind, schema: Schema, records: Seq[Record]): Elem =
    <tbody class={theme_table.className.tbody(kind)}>{table_body_records(kind, schema, records)}</tbody>

  protected def table_body_records(p: Table): Group =
    Group(p.records.toList.map(table_body_record(p, _)))

  protected def table_body_records(kind: TableKind, schema: Schema, records: Seq[Record]): Group =
    Group(records.toList.map(table_body_record(kind, schema, _)))

  protected def table_body_record(p: Table, record: Record): Node = 
    table_record(p, record)

  protected def table_body_record(kind: TableKind, schema: Schema, record: Record): Elem = 
    table_record(kind, schema, record)

  protected def table_body_record_data(kind: TableKind, value: ValueModel): Elem =
    table_data(value)

  protected def table_records(schema: Option[Schema], records: Seq[Record]): Group =
    schema.fold(
      table_records(build_schema(records), records)
    )(
      table_records(_, records)
    )

  protected def table_records(schema: Schema, records: Seq[Record]): Group =
    table_records(strategy.tableKind, schema, records)

  protected def table_records(kind: TableKind, schema: Schema, records: Seq[Record]): Group =
    Group(for (rec <- records) yield table_record(kind, schema, rec))

  protected def table_record(p: Table, record: Record): Node =
    p.kind match {
      case ListTable => table_record_list(p, record)
      case _ => table_record_standard(p, record)
    }

  protected def table_record(schema: Option[Schema], record: Record): Elem =
    schema.fold(
      table_record(build_schema(record), record)
    )(
      table_record(_, record)
    )

  protected def table_record(schema: Schema, record: Record): Elem =
    table_record(strategy.tableKind, schema, record)

  protected def table_record(kind: TableKind, schema: Schema, record: Record): Elem =
    <tr class={theme_table.className.tbodyTr(kind)}>{
      for (c <- schema.columns) yield table_data(kind, c, record)
    }</tr>

  protected def table_record_standard(p: Table, record: Record): Elem = {
    val attrs = SeqUtils.buildTupleVector(
      Vector(
        "class" -> theme_table.className.getTbodyTr(p.kind),
        "data-href" -> table_data_url(p, record)
      )
    )
    val children = for (c <- p.schema.columns) yield {
      table_data(p.kind, c, record)
    }
    XmlUtils.element("tr", attrs, children)
  }

  protected def table_record_list(p: Table, record: Record): Node = {
    val attrs = SeqUtils.buildTupleVector(
      Vector(
        "class" -> theme_table.className.getTbodyTr(p.kind),
        "data-href" -> table_data_url(p, record)
      )
    )
    val icon: Picture = picture_icon(record)
    val title = get_title(record).map(nodeseq).getOrElse(Text(""))
    val subtitle = get_subtitle(record).map(nodeseq).getOrElse(Text(""))
    val content: Node = get_content_summary(record) getOrElse Text("")
    val row1 = List(
      <td class={theme_table.className.tbodyTd(p.kind)} rowspan="2">{table_value_img_picture{icon}}</td>,
      <td class={theme_table.className.tbodyTd(p.kind)}>{title}</td>
    )
    val row2 = List(
      <td class={theme_table.className.tbodyTd(p.kind)}>{content}</td>
    )
    Group(List(
      XmlUtils.element("tr", attrs, row1),
      XmlUtils.element("tr", attrs, row2)
    ))
  }

  protected def table_data_url(p: Table, record: Record): Option[URI] =
    for {
      entitytype <- p.entityType
      id <- record.getOne(PROP_DOMAIN_OBJECT_ID)
    } yield {
      val domainid = domain_entity_id(entitytype, id)
      new URI(s"${entitytype.v}/${domainid.presentationId}.html")
    }

  protected def domain_entity_id(entitytype: DomainEntityType, id: Any): DomainEntityId =
    DomainEntityId(entitytype, StringDomainObjectId(id.toString), None) // TODO

  protected def table_data(kind: TableKind, column: Column, record: Record): Elem =
    <td class={theme_table.className.tbodyTd(kind)}>{table_value(column, record)}</td>

  protected def table_data(v: ValueModel): Elem =
    <td class={theme_table.className.tbodyTd(strategy.tableKind)}>{table_value(v)}</td>

  protected def table_value(column: Column, record: Record): Node =
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
      case XImageLink => table_value_image_link(v)
      case XHtml => table_value_html(v)
      case _ => table_value_string(v)
    }

  protected def get_table_value(column: Column, record: Record): Option[Node] = {
    // TODO datetime formatting
    column.datatype match {
      case XDateTime => table_get_value_datetime(column, record)
      case XDate => table_get_value_date(column, record)
      case XTime => table_get_value_time(column, record)
      case XEverforthid => table_get_value_everforthid(column, record)
      case XLink => table_get_value_link(column, record)
      case XImageLink => table_get_value_image_link(column, record)
      case XHtml => table_get_value_html(column, record)
      case _ => table_get_value_string(column, record)
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

  protected def table_get_value_html(column: Column, record: Record): Option[Node] =
    record.getFormString(column.name).map(table_value_html)

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
      val a = new URI(x)
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
    val style = "width: 256px" // TODO media
    <img src={p.src.toString} alt={alt} style={style}></img>
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

  protected def table_value_string(x: Any): Node = Text(x.toString)

  protected def property_table(schema: Option[Schema], records: Seq[Record]): NodeSeq =
    table(PropertyTable, schema, records)

  protected def property_table(schema: Schema, records: Seq[Record]): NodeSeq =
    table(PropertyTable, schema, records)

  protected def property_sheet(schema: Option[Schema], record: Record): NodeSeq =
    schema.fold(property_sheet(record))(property_sheet(_, record))

  protected def property_sheet(record: Record): NodeSeq = {
    val schema = build_schema(Vector(record))
    property_sheet(schema, record)
  }

  protected def property_sheet(schema: Schema, record: Record): NodeSeq =
    <table class={theme_table.className.table(PropertyTable)}>
  <tbody>{
    for (c <- schema.columns) yield {
      <tr class={theme_table.className.tbodyTr(PropertyTable)}>{
        List(
          <th class={theme_table.className.theadTh(PropertyTable)}>{c.label(locale)}</th>,
          table_data(PropertyTable, c, record)
        )
      }</tr>
    }
  }</tbody>
  </table>

  protected def property_sheet_confirm(): NodeSeq = RAISE.notImplementedYetDefect

  protected def property_form(
    action: URI,
    method: Method,
    schema: Schema,
    record: Record,
    hidden: Hidden,
    submits: Submits
  ): NodeSeq = {
    <form action={action.toString} method={method.name}>
      <table class={theme_table.className.table(FormTable)}>
      <tbody>{
        for (c <- schema.columns) yield {
          <tr class={theme_table.className.tbodyTr(FormTable)}>{
            List(
              <th class={theme_table.className.theadTh(FormTable)}>{c.label(locale)}</th>,
              input_field(c, record)
            )
          }</tr>
        }
      }</tbody>
      </table>
      <table>
        <tr>{
          for (s <- submits.submits) yield {
            <td><input type="submit" value={s.name}></input></td>
          }
        }</tr>
      </table>
      {hidden.render}
    </form>
  }

  protected def input_field(column: Column, record: Record) = {
    if (column.form.readonly)
      table_data(FormTable, column, record)
    else
      <td class={theme_table.className.tbodyTd(FormTable)}>{_input_field(column, record)}</td>
  }

  protected def _input_field(column: Column, record: Record) = {
    column.datatype match {
      case XText => <textarea name={column.name} rows="4"></textarea>
      case _ => <input type="text" name={column.name}></input>
    }
  }

  protected def grid(p: Table): Elem = {
    val ncolumns = 6
    val width = 12 / ncolumns
    <div class="container"> { // container-fluid
      for (row <- p.records.grouped(ncolumns)) yield {
        <div class="row"> {
          for (x <- row) yield {
            <div class={s"col-sm-${width}"}> {
              card(x)
            } </div>
          }
        } </div>
      }
    } </div>
  }

  protected def card(rec: Record): Elem = {
    val icon = picture_icon(rec)
    val title = get_title(rec)
    val subtitle = get_subtitle(rec)
    val header = TitleLine(title, subtitle).toOption
    val content = get_content(rec) getOrElse (Text("-"))
    val c = Card(icon, header, content)
    card(c)
  }

  def card(
    imagetop: Option[Picture],
    header: Option[TitleLine],
    footer: Option[TitleLine],
    content: NodeSeq
  ): Elem = card(Card(imagetop, header, footer, content))

  protected def card(card: Card): Elem =
    strategy.cardKind match {
      case BootstrapCard => card_bootstrap(card)
      case PaperDashboardCard => card_paperdashboard(card)
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
          "alt" -> p.alt.map(node)
        )
      ),
      Nil
    )
    <div class="card"> {
      card.imagetop.map(i =>
        i.href.fold {
          img(i)
        } { href =>
          <a href={href.toString} target="_blank">{img(i)}</a>
        }
      ) ++ card.header.map(h =>
        <div class="card-header">{
          card_title_bootstrap(h)
        }</div>
      ) ++ <div class="card-block">{
        <div class="card-text">{card.content}</div>
      }</div> ++ card.footer.map(f =>
        <div class="card-footer">{
          card_title_bootstrap(f)
        }</div>
      )
    } </div>
  }

  def card_title_bootstrap(p: TitleLine): List[Node] = {
    List(
      p.title.map(t => <h4 class="card-title">{t(locale)}</h4>),
      p.subTitle.map(d => <p class="card-subtitle">{d(locale)}</p>)
    ).flatten
  }

  protected def card_paperdashboard(card: Card): Elem = {
    def img(p: Picture): Elem = XmlUtils.element("img",
      SeqUtils.buildTupleVector(
        Vector(
          "class" -> "card-img-top",
          "src" -> p.src.toString
        ),
        Vector(
          "alt" -> p.alt.map(node)
        )
      ),
      Nil
    )
    <div class="card">{
      card.imagetop.map(i =>
        i.href.fold {
          img(i)
        } { href =>
          <a href={href.toString} target="_blank">{img(i)}</a>
        }
      ) ++ card.header.map(h =>
        <div class="header">{
          List(
            h.title.map(t => <h4 class="title">{t(locale)}</h4>),
            h.subTitle.map(d => <p class="category">{d(locale)}</p>)
          ).flatten
        }</div>
      ) ++ <div class="content">{
        card.content ++ List(
          card.footer.map(f =>
            <div class="footer">{
              List(
                f.title.map(t => <h4 class="title">{t(locale)}</h4>),
                f.subTitle.map(d => <p class="category">{d(locale)}</p>)
              ).flatten
            }</div>
          )
        ).flatten
      }</div>
    }</div>
  }

  def get_title(rec: Record): Option[I18NElement] =
    rec.getString(KEY_DOMAIN_OBJECT_TITLE).map(I18NElement.parse)

  def get_subtitle(rec: Record): Option[I18NElement] =
    rec.getString(KEY_DOMAIN_OBJECT_SUBTITLE).map(I18NElement.parse)

  def get_content(rec: Record): Option[Node] =
    rec.getString(KEY_DOMAIN_OBJECT_CONTENT).map(table_value_html)

  def get_content_summary(rec: Record): Option[Node] =
    rec.getString(KEY_DOMAIN_OBJECT_CONTENT).map(table_value_html_summary)

  def picture_icon(rec: Record): Picture =
    rec.getOne(KEY_DOMAIN_OBJECT_IMAGE_ICON).fold {
      Picture(theme.default.noImageIcon)
    } {
      case m: URL => Picture(m.toURI)
      case m: URI => Picture(m)
      case m: String => Picture(new URI(m))
      case m: Picture => m
      case m => RAISE.noReachDefect
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
    schema: Option[Schema],
    entityType: Option[DomainEntityType],
    records: Option[Seq[Record]]
  )
  object TableOrder {
    def apply(
      kind: TableKind,
      schema: Option[Schema],
      entitytype: DomainEntityType,
      records: Seq[Record]
    ): TableOrder = TableOrder(Some(kind), schema, Some(entitytype), Some(records))

    def apply(
      kind: TableKind,
      schema: Option[Schema],
      entitytype: Option[DomainEntityType],
      records: Seq[Record]
    ): TableOrder = TableOrder(Some(kind), schema, entitytype, Some(records))

    def apply(
      kind: Option[TableKind],
      schema: Option[Schema],
      entitytype: DomainEntityType,
      records: Seq[Record]
    ): TableOrder = TableOrder(kind, schema, Some(entitytype), Some(records))

    def apply(
      kind: Option[TableKind],
      schema: Option[Schema],
      entitytype: Option[DomainEntityType],
      records: Seq[Record]
    ): TableOrder = TableOrder(kind, schema, entitytype, Some(records))
  }

  case class Table(
    kind: TableKind,
    schema: Schema,
    entityType: Option[DomainEntityType],
    records: Seq[Record]
  )
}
