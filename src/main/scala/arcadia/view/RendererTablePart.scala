package arcadia.view

import scala.util.control.NonFatal
import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import java.net.{URI, URL}
import org.goldenport.exception.RAISE
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => _, Table => _, _}
import org.goldenport.xml.XmlUtils
import org.goldenport.xml.dom.DomUtils
import org.goldenport.util.{DateTimeUtils, DateUtils, StringUtils, SeqUtils}
import arcadia.model._
import Renderer._

/*
 * @since   Apr. 15, 2018
 *  version Apr. 15, 2018
 *  version May.  1, 2018
 *  version Sep.  1, 2018
 *  version Nov.  7, 2018
 *  version Apr. 16, 2019
 *  version Oct. 31, 2023
 * @version Dec.  2, 2023
 * @author  ASAMI, Tomoharu
 */
trait RendererTablePart { self: Renderer =>
  protected def table(p: TableOrder): NodeSeq = {
    val records = p.records getOrElse Nil
    val kind = p.kind getOrElse strategy.tableKind
    val size = p.size getOrElse strategy.size
    val schema = strategy.resolveSchema(p)
//    val schema = p.schema getOrElse build_schema(records)
    val entitytype = p.entityType
    val datahref = p.dataHref
    val t = TableWithRecords(kind, size, schema, entitytype, datahref, p.paging, records)
    table(t)
  }

  protected def table(schema: Option[Schema], records: Seq[IRecord]): NodeSeq =
    table(strategy.tableKind, schema, records, None)

  protected def table(records: Seq[IRecord]): NodeSeq = {
    val schema = build_schema(records)
    table(schema, records)
  }

  protected def table(schema: Schema, records: Seq[IRecord]): NodeSeq =
    table(strategy.tableKind, schema, records, None)

  protected def table(kind: TableKind, schema: Option[Schema], records: Seq[IRecord], datahref: Option[URI]): NodeSeq =
    table(TableOrder(Some(kind), None, schema, None, datahref, None, Some(records)))

  protected def table(kind: TableKind, schema: Schema, records: Seq[IRecord], datahref: Option[URI]): NodeSeq =
    table(TableOrder(Some(kind), None, Some(schema), None, datahref, None, Some(records)))

  protected def table(p: TableWithRecords): NodeSeq =
    p.kind match {
      case StandardTable => table_standard(p)
      case ListTable => list_table(p)
      case GridTable => grid(p)
      case _ => table_standard(p)
    }

  protected def table_standard(p: TableWithRecords): NodeSeq = {
    val table = _get_table_page_navigation(p).fold(_table_standard(p))(x =>
      <div>
        {_table_standard(p)}
        {x}
      </div>
    )
    theme_table.container(p.table, table)
  }

  private def _get_table_page_navigation(p: TableWithRecords): Option[Node] =
    p.paging.map(_table_page_navigation(p, _))

  private def _table_page_navigation(
    p: TableWithRecords,
    paging: TableOrder.Paging
  ): Node = {
    val prevLabel = "Prev"
    val nextLabel = "Next"
    val navi = paging.navigation

    def _table_page_navigation_bar_(): NodeSeq = {
      val xs = _table_page_navigation_prev_ +: _table_page_navigation_list_ :+ _table_page_navigation_next_
      NodeSeq.fromSeq(xs)
    }

    def _table_page_navigation_prev_(): Node =
      navi.prev match {
        case Some(s) => <td><a href={s"#${_query_(s)}"}>{prevLabel}</a></td>
        case None => <td>{prevLabel}</td>
      }
    

    def _table_page_navigation_next_(): Node =
      navi.next match {
        case Some(s) => <td><a href={s"#${_query_(s)}"}>{nextLabel}</a></td>
        case None => <td>{nextLabel}</td>
      }

    def _table_page_navigation_list_(): List[Node] =
     for (s <- navi.slots) yield {
        <td><a href={s"#${_query_(s)}"}>{s.numberBase1}</a></td>
      }

    import Renderer.TableOrder.Paging.Navigation.Location

    def _query_(p: Location.Holder) = s"?query.offset=${p.offset}&query.page.size=${p.limit}"

    <table>
      <tr>
        {_table_page_navigation_bar_}
      </tr>
    </table>
  }

  private def _table_standard(p: TableWithRecords): Node =
    <table class={theme_table.css.table(p.table)}>{
      seq(
        caption.map(x => <caption class={theme_table.css.caption(p.table)}>{nodeseq(x)}</caption>),
        Some(table_head(p.table)),
        Some(table_body(p)),
        None.map(x => <tfoot class={theme_table.css.tfoot(p.table)}></tfoot>)
      )
    }</table>

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
      for (c <- p.schema.columns) yield <th scope="col" class={theme_table.css.theadTh(p.tableColumn(c))}>{c.label(locale)}</th>
    }</tr>

  protected def table_body(kind: TableKind, schema: Schema, records: Seq[IRecord]): Elem =
    table_body(TableWithRecords(kind, strategy.size, schema, records))
  //   <tbody class={theme_table.css.tbody(kind)}>{table_body_records(kind, schema, records)}</tbody>

  protected def table_body(p: TableWithRecords): Elem =
    <tbody class={theme_table.css.tbody(p.table)}>{table_body_records(p)}</tbody>

  protected def table_body_records(p: TableWithRecords): Group =
    Group(p.records.toList.map(table_body_record(p.table, _)))

  // protected def table_body_records(kind: TableKind, schema: Schema, records: Seq[Record]): Group =
  //   Group(records.toList.map(table_body_record(kind, schema, _)))

  protected def table_body_record(kind: TableKind, schema: Schema, record: IRecord): Node =
    table_body_record(Table(kind, strategy.size, schema), record)
  //   table_record(kind, schema, record)

  protected def table_body_record(p: Table, record: IRecord): Node = 
    table_record(p, record)

  protected def table_body_record_data(column: Column, kind: TableKind, value: ValueModel): Elem =
    table_data(TableColumn(kind, strategy.size, column), value)

  protected def table_records(schema: Option[Schema], records: Seq[IRecord]): Group =
    schema.fold(
      table_records(build_schema(records), records)
    )(
      table_records(_, records)
    )

  protected def table_records(schema: Schema, records: Seq[IRecord]): Group =
    table_records(strategy.tableKind, schema, records)

  protected def table_records(kind: TableKind, schema: Schema, records: Seq[IRecord]): Group =
    table_records(TableWithRecords(kind, strategy.size, schema, records))

  protected def table_records(p: TableWithRecords): Group =
    Group(for (rec <- p.records) yield table_record(p.table, rec))

  protected def table_record(p: Table, record: IRecord): Node =
    p.kind match {
      case ListTable => table_record_list(p, record)
      case _ => table_record_standard(p, record)
    }

  protected def table_record(schema: Option[Schema], record: IRecord): Node =
    schema.fold(
      table_record(build_schema(record), record)
    )(
      table_record(_, record)
    )

  protected def table_record(schema: Schema, record: IRecord): Node =
    table_record(strategy.tableKind, schema, record)

  protected def table_record(kind: TableKind, schema: Schema, record: IRecord): Node =
    table_record(Table(kind, strategy.size, schema), record)

  protected def table_record_standard(p: Table, record: IRecord): Elem = {
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

  protected def table_record_list(p: Table, record: IRecord): Node = {
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

  protected def table_data(p: Table, column: Column, record: IRecord): Elem =
    table_data(p.tableColumn(column), record)

  protected def table_data(p: TableColumn, record: IRecord): Elem =
    <td class={theme_table.css.tbodyTd(p)}>{table_value(p, record)}</td>

  protected def table_data(p: TableColumn, v: ValueModel): Elem =
    <td class={theme_table.css.tbodyTd(p)}>{table_value(v)}</td>

  protected def table_value(column: TableColumn, record: IRecord): NodeSeq =
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

  protected def get_table_value(column: TableColumn, record: IRecord): Option[NodeSeq] = {
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
      case XRecordInstance => table_get_value_record(c, record)
      case _ => table_get_value_string(c, record)
    }
  }

  protected def table_get_value_datetime(column: Column, record: IRecord): Option[Node] =
    record.getTimestamp(column.name).map(table_value_datetime)

  protected def table_get_value_date(column: Column, record: IRecord): Option[Node] =
    record.getDate(column.name).map(table_value_date)
//     record.getDate(column.name) flatMap {
//       case m: java.util.Date => Some(Text(DateUtils.toIsoDateString(m)))
// //      case m: DateTime => DateUtils.toIsoDateString(m)
// //      case m: LocalDate => DateUtils.toIsoDateString(m)
//       case m => table_get_value_string(column, record)
//     }

  protected def table_get_value_time(column: Column, record: IRecord): Option[Node] =
    table_get_value_string(column, record) // XXX

  protected def table_get_value_everforthid(column: Column, record: IRecord): Option[Node] =
    record.getString(column.name).map(table_value_everforthid)

  protected def table_get_value_link(column: Column, record: IRecord): Option[Node] =
    record.get(column.name).map(table_value_link)

  protected def table_get_value_image_link(column: Column, record: IRecord): Option[Node] =
    record.get(column.name).map(table_value_image_link)

  protected def table_get_value_img(column: TableColumn, record: IRecord): Option[Node] =
    record.get(column.name).map(table_value_img(column, _))

  protected def table_get_value_html(column: Column, record: IRecord): Option[Node] =
    record.getString(column.name).map(table_value_html)

  protected def table_get_value_text(column: Column, record: IRecord): Option[Node] =
    record.getString(column.name).map(table_value_text)

  protected def table_get_value_record(column: Column, record: IRecord): Option[NodeSeq] =
    record.takeRecordList(column.name) match {
      case Nil => None
      case x :: Nil => Some(property_sheet(x))
      case xs => Some(property_sheet_list(xs))
    }

  protected def table_get_value_string(column: Column, record: IRecord): Option[Node] =
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

  protected def table_value_image_link_picture(p: Picture): Node = p match {
    case m: Picture.UriPicture => table_value_image_link_picture(m)
    case m: Picture.IconPicture => table_value_image_link_picture(m)
  }

  protected def table_value_image_link_picture(p: Picture.UriPicture): Node = {
    val src = p.src.toString
    val a = p.alt.map(string).getOrElse(src)
    val s = StringUtils.pathLastComponentBody(a)
    <span data-toggle="tooltip" title={src}>{s}</span>
  }

  protected def table_value_image_link_picture(p: Picture.IconPicture): Node = {
    RAISE.notImplementedYetDefect
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

  protected def table_value_img_picture(column: TableColumn, p: Picture): Node =
    p match {
      case m: Picture.UriPicture => table_value_img_picture(m)
      case m: Picture.IconPicture => table_value_img_picture(m)
    }

  protected def table_value_img_picture(column: TableColumn, p: Picture.UriPicture): Node = {
    val alt: String = p.alt.map(string).getOrElse("")
    val src = column.kind match {
      case PropertyTable => p.l
      case _ => p.xs
    }
    <img class={theme_table.css.img(strategy.tableKind)} src={src} alt={alt}></img>
  }

  protected def table_value_img_picture(column: TableColumn, p: Picture.IconPicture): Node = {
    RAISE.notImplementedYetDefect
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

  protected def table_value_img_picture(p: Picture): Node = p match {
    case m: Picture.UriPicture => table_value_img_picture(m)
    case m: Picture.IconPicture => table_value_img_picture(m)
  }

  protected def table_value_img_picture(p: Picture.UriPicture): Node = {
    val alt: String = p.alt.map(string).getOrElse("")
    val src = strategy.tableKind match {
      case PropertyTable => p.l
      case _ => p.xs
    }
    <img class={theme_table.css.img(strategy.tableKind)} src={src} alt={alt}></img>
  }

  protected def table_value_img_picture(p: Picture.IconPicture): Node = {
    RAISE.notImplementedYetDefect
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
}
