package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import scala.util.control.NonFatal
import java.util.Locale
import java.net.URI
import org.joda.time.DateTime
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import arcadia.model._
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.util.{DateTimeUtils, DateUtils, StringUtils}
import org.goldenport.record.v2.util.RecordUtils

/*
 * @since   Aug.  1, 2017
 *  version Aug. 29, 2017
 * @version Sep. 23, 2017
 * @author  ASAMI, Tomoharu
 */
abstract class Renderer(
  strategy: RenderStrategy,
  pageName: Option[I18NString],
  headline: Option[I18NElement],
  title: Option[I18NElement],
  caption: Option[I18NElement]
) {
  lazy val l10n_page_name = pageName.map(string)
  lazy val l10n_headline = headline.map(nodeseq)
  lazy val l10n_title = title.map(nodeseq)
  lazy val l10n_caption = caption.map(nodeseq)
  lazy val effective_page_name = l10n_page_name orElse l10n_headline orElse l10n_title orElse l10n_caption
  lazy val effective_headline = l10n_headline orElse l10n_headline
  def locale = strategy.locale
  def theme = strategy.theme
  lazy val theme_head = theme.head
  lazy val theme_table = theme.table

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

  protected def nodeseq(s: I18NElement): NodeSeq = s.apply(locale)

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
  
  protected def table(kind: TableKind, schema: Schema, records: Seq[Record]): NodeSeq = theme_table.container(kind, schema, records,
    <table class={theme_table.className.table(kind)}>{
      seq(
        caption.map(x => <caption class={theme_table.className.caption(kind)}>{nodeseq(x)}</caption>),
        Some(<thead class={theme_table.className.thead(kind)}>{table_head(kind, schema)}</thead>),
        Some(<tbody class={theme_table.className.tbody(kind)}>{table_records(kind, schema, records)}</tbody>),
        None.map(x => <tfoot class={theme_table.className.tfoot(kind)}></tfoot>)
      )
    }</table>
  )

  protected def table_head(kind: TableKind, schema: Schema): Elem = <tr class={theme_table.className.theadTr(kind)}>{
    for (c <- schema.columns) yield <th class={theme_table.className.theadTh(kind)}>{c.label(locale)}</th>
  }</tr>

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

  protected def table_data(kind: TableKind, column: Column, record: Record): Elem =
    <td class={theme_table.className.tbodyTd(kind)}>{table_value(column, record)}</td>

  protected def table_value(column: Column, record: Record): Node =
    get_table_value(column, record).getOrElse(Text(""))

  protected def get_table_value(column: Column, record: Record): Option[Node] = {
    // TODO datetime formatting
    column.datatype match {
      case XDateTime => table_get_value_datetime(column, record)
      case XDate => table_get_value_date(column, record)
      case XTime => table_get_value_time(column, record)
      case XEverforthid => table_get_value_everforthid(column, record)
      case XLink => table_get_value_link(column, record)
      case _ => table_get_value_string(column, record)
    }
  }

  protected def table_get_value_datetime(column: Column, record: Record): Option[Node] = {
    def print(label: String, jst: String, gmt: String) = 
      <span data-toggle="tooltip" title={s"$jst(JST) $gmt(GMT)"}>{label}</span>
    record.getFormTimestamp(column.name) map { x => 
      print(
//        DateTimeUtils.toSimpleString24Jst(x),
        DateTimeUtils.toSimpleStringJst(x),
        DateTimeUtils.toSimpleStringJst(x),
        DateTimeUtils.toSimpleStringGmt(x)
      )
    }
  }

  protected def table_get_value_date(column: Column, record: Record): Option[Node] =
    record.getFormDate(column.name) flatMap {
      case m: java.util.Date => Some(Text(DateUtils.toIsoDateString(m)))
//      case m: DateTime => DateUtils.toIsoDateString(m)
//      case m: LocalDate => DateUtils.toIsoDateString(m)
      case m => table_get_value_string(column, record)
    }

  protected def table_get_value_time(column: Column, record: Record): Option[Node] =
    table_get_value_string(column, record) // XXX

  protected def table_get_value_everforthid(column: Column, record: Record): Option[Node] =
    record.getString(column.name) map { id =>
      val s = id.takeRight(3)
//      <a href={id} data-toggle="tooltip" title={id} data-placement="auto right">{s}</a>
      val text = s"""<a href="$id">$id</a>"""
      <span data-toggle="popover" data-content={text} data-html="true">{s}</span>
    }

  protected def table_get_value_link(column: Column, record: Record): Option[Node] =
    record.getFormString(column.name) map { x =>
      val s = try {
        val a = new URI(x)
        StringUtils.pathLastComponentBody(a.getPath())
      } catch {
        case NonFatal(e) => x
      }
      <span data-toggle="tooltip" title={x}>{s}</span>
    }

  protected def table_get_value_string(column: Column, record: Record): Option[Node] =
    record.getString(column.name).map(Text(_))

  protected def property_table(schema: Option[Schema], records: Seq[Record]): NodeSeq =
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

  protected def card(
    imagetop: Option[ImageAlt],
    header: Option[TitleDescription],
    footer: Option[TitleDescription],
    content: NodeSeq
  ): Elem = {
    <div class="card">{
      header.map(h =>
        <div class="header">{
          List(
            h.title.map(t => <h4 class="title">{t(locale)}</h4>),
            h.description.map(d => <p class="category">{d(locale)}</p>)
          ).flatten
        }</div>
      ) ++ <div class="content">{
        content ++ List(
          footer.map(f =>
            <div class="footer">{
              List(
                f.title.map(t => <h4 class="title">{t(locale)}</h4>),
                f.description.map(d => <p class="category">{d(locale)}</p>)
              ).flatten
            }</div>
          )
        ).flatten
      }</div>
    }</div>
  }

  /*
   * Utilities
   */
  protected final def build_schema(record: Record): Schema =
    RecordUtils.buildSchema(Vector(record))

  protected final def build_schema(rs: Seq[Record]): Schema =
    RecordUtils.buildSchema(rs)
}
