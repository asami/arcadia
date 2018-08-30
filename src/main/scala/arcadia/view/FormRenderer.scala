package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import java.util.Locale
import java.net.{URI, URL}
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.xml.XmlUtils
import org.goldenport.util.{DateTimeUtils, DateUtils, StringUtils, SeqUtils}
import arcadia._
import arcadia.context.Parameter
import arcadia.model._
import Renderer._

/*
 * @since   Jul.  2, 2018
 * @version Jul. 23, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait FormRenderer {
  def strategy: RenderStrategy
  def form: InputForm
  protected def class_body: String
  protected def class_row: String
  protected def class_field(f: FormRenderer.Field): String
  protected def class_label: String
  protected def class_input: String
  protected def class_submit: String
  protected def class_button_row: String
  protected def class_button: String
  protected def input_type(c: FormRenderer.Field): String
  protected def input_value(c: FormRenderer.Field, v: Any): String

  protected def is_query = form.isQuery
  def locale = strategy.locale

  protected def generate_id() = strategy.generateId()
  protected def add_javascript_in_footer(p: String): Unit = strategy.addJavaScriptInFooter(p)

  protected lazy val placeholder_start = strategy.label.placeholderStart.toI18NString.apply(locale)
  protected lazy val placeholder_end = strategy.label.placeholderEnd.toI18NString.apply(locale)

  def apply = <form method="GET" action={form.action.toString}>{
    body
  }</form>

  def body: Node = <div class={class_body}>{
    rows
  }</div>

  def hiddens: Seq[Elem] = form.hiddens.toKeyValues.map {
    case (k, v) => <input type="hidden" name={k} value={v}></input>
  }

  def rows: List[NodeSeq] = build_rows.rows.map(row) ++ buttons

  def row(p: FormRenderer.Row): Node = <div class={class_row}>{
    p.fields.map(field)
  }</div>

  def buttons: Node = <div class={class_button_row}>{
    form.submits.submits.map(x => <div class={class_button}>{button_submit(x)}</div>)
  }</div>

  protected def button_submit(p: Submit): Elem =
    <input type="submit" class={class_submit} name={p.name} value={p.value(locale)}/>

  def field(c: FormRenderer.Field): Elem = {
    val id = generate_id()
    <div class={class_field(c)}>{List(label(c, id), input(c, id))}</div>
  }
  def label(c: FormRenderer.Field, inputid: String): Elem = <label class={class_label} for={inputid}>{c.label(locale)}</label>
  def input(c: FormRenderer.Field, inputid: String): Elem =
    if (is_query)
      c.datatype match {
        case XDateTime => query_datetime(c, inputid)
        case XDate => query_date(c, inputid)
        case XTime => query_time(c, inputid)
        case m: XPowertype => query_powertype(c, inputid, m)
        case XImageLink => query_imagelink(c, inputid)
        case XFile => query_file(c, inputid)
        case _ => query_plain(c, inputid)
      }
      else
        c.datatype match {
          case XDateTime => input_datetime(c, inputid)
          case XDate => input_date(c, inputid)
          case XTime => input_time(c, inputid)
          case m: XPowertype => input_powertype(c, inputid, m)
          case XImageLink => input_imagelink(c, inputid)
          case XFile => input_file(c, inputid)
          case _ => input_plain(c, inputid)
        }

  protected def query_datetime(c: FormRenderer.Field, inputid: String): Elem = input_datetime(c, inputid)
  protected def query_date(c: FormRenderer.Field, inputid: String): Elem = input_datetime(c, inputid)
  protected def query_time(c: FormRenderer.Field, inputid: String): Elem = input_datetime(c, inputid)
  protected def query_powertype(c: FormRenderer.Field, inputid: String, pt: XPowertype): Elem = input_powertype(c, inputid, pt)
  protected def query_imagelink(c: FormRenderer.Field, inputid: String): Elem = input_imagelink(c, inputid)
  protected def query_file(c: FormRenderer.Field, inputid: String): Elem = input_file(c, inputid)
  protected def query_plain(c: FormRenderer.Field, inputid: String): Elem = input_plain(c, inputid)

  protected def input_datetime(c: FormRenderer.Field, inputid: String): Elem = input_plain(c, inputid)
  protected def input_date(c: FormRenderer.Field, inputid: String): Elem = input_plain(c, inputid)
  protected def input_time(c: FormRenderer.Field, inputid: String): Elem = input_plain(c, inputid)
  protected def input_powertype(c: FormRenderer.Field, inputid: String, m: XPowertype): Elem = input_plain(c, inputid)
  protected def input_imagelink(c: FormRenderer.Field, inputid: String): Elem = input_plain(c, inputid)
  protected def input_file(c: FormRenderer.Field, inputid: String): Elem = input_plain(c, inputid)

  protected def input_plain(c: FormRenderer.Field, inputid: String): Elem =
    XmlUtils.appendAttributes(
      <input type={input_type(c)} class={class_input} id={inputid} name={c.name}/>,
      "value" -> form.getValue(c.name).map(input_value(c, _)),
      "placeholder" -> c.form.placeholder.map(_(locale))
    )

  def inputType(c: FormRenderer.Field): String = input_type(c)

  protected def build_rows: FormRenderer.Rows
}
object FormRenderer {
  case class Rows(rows: List[Row])
  object Rows {
    def apply(ps: Seq[Row]): Rows = Rows(ps.toList)
  }

  case class Row(fields: List[Field])
  object Row {
    def apply(ps: Seq[Field]): Row = Row(ps.toList)
  }

  case class Field(c: Column, width: Int) {
    def name = c.name
    def datatype = c.datatype
    def label(p: Locale) = c.label(p)
    def form = c.form
  }
}

// Bootstrap 4
sealed trait BootstrapFormRenderer extends FormRenderer {
  protected def class_body: String = "container"
  protected def class_row: String = "form-row align-items-center"
  protected def class_field(c: FormRenderer.Field): String = "form-group col-auto"
  protected def class_label: String = "col-form-label"
  protected def class_input: String = "form-control"
  protected def class_submit: String = "btn btn-primary"
  protected def class_button_row: String = "form-row align-items-center justify-content-end"
  protected def class_button: String = "col-auto"
  protected def input_type(c: FormRenderer.Field): String = c.datatype match {
    case XImageLink => "file"
    case XFile => "file"
    case _ => "text"
  }
  protected def input_value(c: FormRenderer.Field, v: Any): String = v.toString

  override protected def query_datetime(c: FormRenderer.Field, inputid: String): Elem = {
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
    <div class="input-group date col-auto" id={idstart}>{
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
    <div class="input-group date col-auto" id={idend}>{
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

  override protected def input_datetime(c: FormRenderer.Field, inputid: String): Elem = {
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
    <div class="input-group date col-sm-5" id={idproperty}>{
      List(
        XmlUtils.appendAttributes(
          <input type="text" name={property} class="form-control" id={inputid} />,
          "placeholder" -> None
        ),
        <span class="input-group-addon">
          <i class="fa fa-times"/>
          <i class="fa fa-calendar"/>
        </span>
      )
    }</div>
  }
  override protected def input_date(c: FormRenderer.Field, inputid: String): Elem =
    super.input_date(c, inputid)
  override protected def input_time(c: FormRenderer.Field, inputid: String): Elem =
    super.input_time(c, inputid)
  override protected def input_powertype(c: FormRenderer.Field, inputid: String, pt: XPowertype): Elem = {
    <div class="col-sm-10">
    <select class="custom-select" id={inputid}>{
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
  }
  override protected def input_imagelink(c: FormRenderer.Field, inputid: String): Elem =
    super.input_imagelink(c, inputid)
}

case class AutoBootstrapFormRenderer(
  strategy: RenderStrategy,
  form: InputForm
) extends BootstrapFormRenderer {
  protected def build_rows: FormRenderer.Rows = FormRenderer.Rows(List(FormRenderer.Row(form.schema.columns.map(FormRenderer.Field(_, 1)))))
}

sealed trait NowUiFormRenderer extends BootstrapFormRenderer {
  override protected def query_datetime(c: FormRenderer.Field, inputid: String): Elem = {
    val startproperty = s"${c.name}.start"
    val endproperty = s"${c.name}.end"
    <div class="form-group row align-items-center">{
      List(
        XmlUtils.appendAttributes(
          <input type="text" class="form-control col datetimepicker" id={inputid} name={startproperty}/>,
          "value" -> form.getValue(c.name).map(input_value(c, _)),
          "placeholder" -> c.form.placeholder.map(_(locale))
        ),
        <span class="ml-1 mr-1">〜</span>,
        XmlUtils.appendAttributes(
          <input type="text" class="form-control col datetimepicker" name={endproperty}/>,
          "value" -> form.getValue(c.name).map(input_value(c, _)),
          "placeholder" -> c.form.placeholder.map(_(locale))
        )
      )
    }</div>
  }

  override protected def query_date(c: FormRenderer.Field, inputid: String): Elem = {
    val startproperty = s"${c.name}.start"
    val endproperty = s"${c.name}.end"
    <div class="form-group row align-items-center">{
      List(
        XmlUtils.appendAttributes(
          <input type="text" class="form-control col datepicker" id={inputid} name={startproperty}/>,
          "value" -> form.getValue(c.name).map(input_value(c, _)),
          "placeholder" -> c.form.placeholder.map(_(locale))
        ),
        <span class="ml-1 mr-1">〜</span>,
        XmlUtils.appendAttributes(
          <input type="text" class="form-control col datepicker" name={endproperty}/>,
          "value" -> form.getValue(c.name).map(input_value(c, _)),
          "placeholder" -> c.form.placeholder.map(_(locale))
        )
      )
    }</div>
  }

  override protected def query_time(c: FormRenderer.Field, inputid: String): Elem = {
    val startproperty = s"${c.name}.start"
    val endproperty = s"${c.name}.end"
    <div class="form-group row">{
      List(
        XmlUtils.appendAttributes(
          <input type="text" class="form-control timepicker" id={inputid} name={startproperty}/>,
          "value" -> form.getValue(c.name).map(input_value(c, _)),
          "placeholder" -> c.form.placeholder.map(_(locale))
        ),
        XmlUtils.appendAttributes(
          <input type="text" class="form-control timepicker" name={endproperty}/>,
          "value" -> form.getValue(c.name).map(input_value(c, _)),
          "placeholder" -> c.form.placeholder.map(_(locale))
        )
      )
    }</div>
  }

  override protected def input_datetime(c: FormRenderer.Field, inputid: String): Elem =
    XmlUtils.appendAttributes(
      <input type="text" class="form-control datetimepicker" id={inputid} name={c.name}/>,
      "value" -> form.getValue(c.name).map(input_value(c, _)),
      "placeholder" -> c.form.placeholder.map(_(locale))
    )

  override protected def input_date(c: FormRenderer.Field, inputid: String): Elem =
    XmlUtils.appendAttributes(
      <input type="text" class="form-control datepicker" id={inputid} name={c.name}/>,
      "value" -> form.getValue(c.name).map(input_value(c, _)),
      "placeholder" -> c.form.placeholder.map(_(locale))
    )

  override protected def input_time(c: FormRenderer.Field, inputid: String): Elem =
    XmlUtils.appendAttributes(
      <input type="text" class="form-control timepicker" id={inputid} name={c.name}/>,
      "value" -> form.getValue(c.name).map(input_value(c, _)),
      "placeholder" -> c.form.placeholder.map(_(locale))
    )

  override protected def input_powertype(c: FormRenderer.Field, inputid: String, pt: XPowertype): Elem = {
    <select class="selectpicker" id={inputid} data-style="btn btn-secondary btn-round">{
      List(
        c.form.placeholder.map(x =>
          <option selected="">{x(locale)}</option>
        ).getOrElse(
          <option selected=""></option>
        ),
        for (t <- pt.powertype.elements) yield {
          <option value={t.name}>{t.label}</option>
        }
      )
    }
    </select>
  }
}

case class AutoNowUiFormRenderer(
  strategy: RenderStrategy,
  form: InputForm
) extends NowUiFormRenderer {
  protected def build_rows: FormRenderer.Rows = FormRenderer.Rows(List(FormRenderer.Row(form.schema.columns.map(FormRenderer.Field(_, 1)))))
}

trait AutoFormRendererFeature { self: FormRenderer =>
  protected def row_Width: Int

  override def rows: List[NodeSeq] = {
    val rs = build_rows
    if (is_single_line(rs))
      single_line(rs)
    else
      multi_line(rs)
  }

  protected def is_single_line(ps: FormRenderer.Rows) = {
    val a = form.submits.submits.length < 2
    val b = ps.rows match {
      case Nil => true
      case x :: Nil if x.fields.map(_.width).sum < row_Width - 1 => true
      case _ => false
    }
    a && b
  }

  protected def single_line(ps: FormRenderer.Rows): List[NodeSeq] =
    List(
      <div class={class_button_row}>{
        val a: List[Elem] = ps.rows.head.fields.map(field)
        val b: Elem = <div class={class_button}>{List(button_submit(form.submits.submits.head))}</div>
        a :+ b
      }</div>
    )

  private def multi_line(ps: FormRenderer.Rows): List[NodeSeq] =
    ps.rows.map(row) ++ buttons

  protected def build_rows: FormRenderer.Rows = {
    case class Z(
      fields: Vector[FormRenderer.Field] = Vector.empty,
      rows: Vector[FormRenderer.Row] = Vector.empty
    ) {
      def r = {
        val a = if (fields.isEmpty)
          rows
        else
          rows :+ FormRenderer.Row(fields)
        FormRenderer.Rows(a)
      }
      def +(rhs: Column) = {
        val length = _length(rhs)
        val f = FormRenderer.Field(rhs, length)
        val nfields = _fields_length + length
        if (nfields < row_Width)
          copy(fields = fields :+ f)
        else if (nfields == row_Width)
          copy(rows = rows :+ FormRenderer.Row(fields :+ f))
        else
          Z(Vector(f), rows :+ FormRenderer.Row(fields))
      }

      private def _fields_length = fields.map(_.width).sum
    }
    form.schema.columns.toList./:(Z())(_+_).r
  }

  private def _length(c: Column) =
    if (is_query)
      _length_query(c)
    else
      _length_command(c)

  private def _length_query(c: Column) = c.datatype match {
    case XDateTime => 2
    case XDate => 2
    case XTime => 2
    case _ => 1
  }

  private def _length_command(c: Column) = 1
}

trait AutoNNowUiFormRenderer extends NowUiFormRenderer with AutoFormRendererFeature {
  val totalLength = 12
  protected lazy val colLength = totalLength / row_Width
  val submitLengthInSingleLine = 3
  val submitLengthInSingleLineMin = 2
  val inputLengthInSingleLine = totalLength - submitLengthInSingleLine
  val inputLengthInSingleLineMax = totalLength - submitLengthInSingleLineMin

  override protected def class_field(c: FormRenderer.Field): String =
    if (is_query && c.datatype == XDateTime)
      "col-auto ml-3 mr-3"
    else
      "form-group col-auto"

  private case class Slot(field: FormRenderer.Field, length: Int) {
    def column = field.c
    def datatype = field.datatype
    def addLength(p: Int) = copy(length = length + p)
  }

  override protected def single_line(ps: FormRenderer.Rows): List[NodeSeq] = {
    def f(slot: Slot): Elem = {
      val id = generate_id()
      <div class={s"form-group col-${slot.length}"}>{
        List(label(slot.field, id), input(slot.field, id))
      }</div>
    }
    val xs = _widen(ps.rows.head.fields)
    val submitlength = totalLength - xs.map(_.length).sum
    List(
      <div class={class_button_row}>{
        val a: List[Elem] = xs.map(f)
        val b: Elem = <div class={s"col-${submitlength} center-block text-center"}>{List(button_submit(form.submits.submits.head))}</div>
        a :+ b
      }</div>
    )
  }

  private def _widen(ps: List[FormRenderer.Field]): List[Slot] = {
    @annotation.tailrec
    def go(xs: List[Slot]): List[Slot] = {
      val width = xs.map(_.length).sum
      if (inputLengthInSingleLine <= width) {
        xs
      } else {
        val xs2 = _update(xs)
        val width2 = xs.map(_.length).sum
        if (inputLengthInSingleLineMax <= width2)
          _adjust(xs)
        else
          go(xs2)
      }
    }
    go(ps.map(x => Slot(x, x.width * colLength)))
  }

  private def _update(ps: List[Slot]) =
    if (is_query)
      _update_query(ps)
    else
      _update_mutation(ps)

  private def _update_query(ps: List[Slot]) = ps map { slot =>
    slot.datatype match {
      case XDateTime => slot
      case XDate => slot
      case XTime => slot
      case _: XPowertype => slot
      case _ => slot.addLength(1)
    }
  }

  private def _update_mutation(ps: List[Slot]) = ps map { slot =>
    slot.addLength(1)
  }

  private def _adjust(xs: List[Slot]) = xs // XXX
}

case class Auto4NowUiFormRenderer(
  strategy: RenderStrategy,
  form: InputForm
) extends AutoNNowUiFormRenderer {
  protected def row_Width = 4
}
