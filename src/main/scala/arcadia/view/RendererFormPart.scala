package arcadia.view

import scala.xml.{NodeSeq, Group, Elem, Node, Text}
import java.util.Locale
import java.net.{URI, URL}
import org.goldenport.exception.RAISE
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => _, _}
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.xml.XmlUtils
import org.goldenport.util.{DateTimeUtils, DateUtils, StringUtils, SeqUtils}
import arcadia._
import arcadia.context.Parameter
import arcadia.model._
import arcadia.controller.Controller.PROP_REDIRECT
import Renderer._

/*
 * @since   Apr. 22, 2018
 *  version May. 14, 2018
 *  version Jul. 23, 2018
 * @version Sep.  1, 2018
 * @author  ASAMI, Tomoharu
 */
trait RendererFormPart { self: Renderer =>
  protected def property_input_form(
    action: URI,
    method: Method,
    schema: Schema,
    record: IRecord,
    hiddens: Hiddens,
    submits: Submits
  ): NodeSeq = strategy.theme match {
    case m: Bootstrap4RenderThemeBase =>
      property_input_form(
        InputForm(action, method, schema, record, hiddens, submits))
    case m =>
      property_input_form_table(action, method, schema, record, hiddens, submits)
  }

  protected def property_input_form(p: InputForm) = strategy.theme match {
    case m: Bootstrap4RenderThemeBase => property_input_form_bootstrap4(p)
    case m: Bootstrap3RenderThemeBase => property_input_form_bootstrap3(p)
    case m => RAISE.notImplementedYetDefect
  }

  protected def property_input_form_bootstrap4(p: InputForm): NodeSeq =
    Auto4NowUiFormRenderer(strategy, p).apply // XXX

  protected def property_input_form_bootstrap4_old(p: InputForm): NodeSeq = {
    <div class="container">
    <form method="GET" action="">{
      val a = for (c <- p.schema.columns) yield property_input_form_item_row_bootstrap4(c)
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

  protected def property_input_form_bootstrap3(p: InputForm): NodeSeq = {
    <div class="container">
    <form method="GET" action="">{
      val a = for (c <- p.schema.columns) yield property_input_form_item_row_bootstrap3(c)
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

  protected def property_input_form_item_row_bootstrap4(c: Column): Elem = {
    val id = generate_id()
    <div class="form-group row">
    <label class="col-form-label col-sm-2" for={id}>{c.label(locale)}</label>
    {property_input_form_item_bootstrap4(c, id)}
    </div>
  }

  protected def property_input_form_item_row_bootstrap3(c: Column): Elem = {
    val id = generate_id()
    <div class="form-group row">
    <label class="col-form-label col-sm-2" for={id}>{c.label(locale)}</label>
    {property_input_form_item_bootstrap3(c, id)}
    </div>
  }

  protected def property_input_form_item(p: Column, id: String) = strategy.theme match {
    case m: Bootstrap4RenderThemeBase => property_input_form_item_bootstrap4(p, id)
    case m: Bootstrap3RenderThemeBase => property_input_form_item_bootstrap3(p, id)
    case m => RAISE.notImplementedYetDefect
  }

  protected def property_input_form_item_bootstrap4(c: Column, id: String): Elem = 
    c.datatype match {
      case XDateTime => _property_input_form_item_datetime_bootstrap4(c, id)
      case m: XPowertype => _property_input_form_item_powertype_bootstrap4(c, id, m)
      case XImageLink => _property_input_form_item_imagelink_bootstrap4(c, id)
      case XFile => _property_input_form_item_file_bootstrap4(c, id)
      case _ => _property_input_form_item_bootstrap4(c, id)
    }

  private def _property_input_form_item_datetime_bootstrap4(c: Column, id: String): Elem = {
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
          <input type="text" name={property} class="form-control" id={id} />,
          "placeholder" -> None
        ),
        <span class="input-group-addon">
          <i class="fa fa-times"/>
          <i class="fa fa-calendar"/>
        </span>
      )
    }</div>
  }

  private def _property_input_form_item_powertype_bootstrap4(c: Column, id: String, pt: XPowertype): Elem = {
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
  }

  private def _property_input_form_item_imagelink_bootstrap4(c: Column, id: String): Elem = {
    <div class="col-sm-10">
      <input type="file" class="form-control" id={id} name={c.name} />
    </div>
  }

  private def _property_input_form_item_file_bootstrap4(c: Column, id: String): Elem = {
    <div class="col-sm-10">
      <input type="file" class="form-control" id={id} name={c.name} />
    </div>
  }

  private def _property_input_form_item_bootstrap4(c: Column, id: String): Elem = {
    <div class="col-sm-10">{
      XmlUtils.appendAttributes(
        <input type={_form_type(c)} class="form-control" id={id} name={c.name} />,
        "placeholder" -> c.form.placeholder.map(_(locale))
      )
    }</div>
  }

  protected def property_input_form_item_bootstrap3(c: Column, id: String): Elem = 
    c.datatype match {
      case XDateTime => _property_input_form_item_datetime_bootstrap3(c, id)
      case m: XPowertype => _property_input_form_item_powertype_bootstrap3(c, id, m)
      case XImageLink => _property_input_form_item_imagelink_bootstrap3(c, id)
      case XFile => _property_input_form_item_file_bootstrap3(c, id)
      case _ => _property_input_form_item_bootstrap3(c, id)
    }

  private def _property_input_form_item_datetime_bootstrap3(c: Column, id: String): Elem = {
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
          <input type="text" name={property} class="form-control" id={id} />,
          "placeholder" -> None
        ),
        <span class="input-group-addon">
          <i class="fa fa-times"/>
          <i class="fa fa-calendar"/>
        </span>
      )
    }</div>
  }

  private def _property_input_form_item_powertype_bootstrap3(c: Column, id: String, pt: XPowertype): Elem = {
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
  }

  private def _property_input_form_item_imagelink_bootstrap3(c: Column, id: String): Elem = {
    <div class="col-sm-10">
      <input type="file" class="form-control" id={id} name={c.name} />
    </div>
  }

  private def _property_input_form_item_file_bootstrap3(c: Column, id: String): Elem = {
    <div class="col-sm-10">
      <input type="file" class="form-control" id={id} name={c.name} />
    </div>
  }

  private def _property_input_form_item_bootstrap3(c: Column, id: String): Elem = {
    <div class="col-sm-10">{
      XmlUtils.appendAttributes(
        <input type={_form_type(c)} class="form-control" id={id} name={c.name} />,
        "placeholder" -> c.form.placeholder.map(_(locale))
      )
    }</div>
  }

  protected def property_input_form_table(
    action: URI,
    method: Method,
    schema: Schema,
    record: IRecord,
    hiddens: Hiddens,
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
              <th scope="row" class={theme_table.css.theadTh(tc)}>{c.label(locale)}</th>,
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
      {hidden_fields(hiddens)}
    </form>
  }

  protected def input_field(tc: TableColumn, record: IRecord) = {
    val column = tc.column
    if (column.form.readonly)
      table_data(tc, record)
    else
      <td class={theme_table.css.tbodyTd(tc)}>{_input_field(column, record)}</td>
  }

  private def _input_field(column: Column, record: IRecord) = {
    val s = record.getString(column.name) getOrElse ""
    column.datatype match {
      case XText => <textarea name={column.name} rows="4" value={s}></textarea>
      case _ => <input type="text" name={column.name} value={s}></input>
    }
  }

  protected def hidden_fields(p: Hiddens) = Group(
    p.toKeyValues.map {
      case (k, v) => <input type="hidden" name={k} value={v}></input>
    }
  )

  protected def get_hidden_data(column: Column, record: IRecord) =
    record.getString(column.name).map(s =>
      <input type="hidden" name={column.name} value={s}></input>
    )

  protected def property_confirm_form(
    action: URI,
    method: Method,
    schema: Schema,
    record: IRecord,
    hiddens: Hiddens,
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
              <th scope="row" class={theme_table.css.theadTh(tc)}>{c.label(locale)}</th>,
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
      {hidden_fields(hiddens)}
    </form>
  }

  protected def update_entity_directive_form(
    action: URI,
    record: IRecord
  ): NodeSeq = {
    // TODO see ButtonTag
    <form action={action.toString} method="PUT"> {
    }
    </form>
  }

  protected def invoke_with_id_directive_form(
    action: URI,
    record: IRecord
  ): NodeSeq = {
    // TODO see ButtonTag
    <form action={action.toString} method="PUT"> {
    }
    </form>
  }

  protected def searchbox_form(p: SearchBox): NodeSeq =
    property_input_form(p.input)

  // Bootstrap4
  private def searchbox_form_old(p: SearchBox): Elem = {
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
    <label class="col-form-label col-sm-2" for={id}>{c.label(locale)}</label>
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
    <label class="control-label col-sm-2" for={id}>{c.label(locale)}</label>
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
    <label class="control-label col-sm-2" for={id}>{c.label(locale)}</label>
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

  protected def command_form(
    parcel: Parcel,
    method: Method,
    action: String,
    title: Option[I18NElement],
    description: Option[I18NElement],
    submitname: String,
    parameters: List[Parameter],
    isactive: Boolean,
    isreturnback: Boolean
  ): Elem = {
    val buttonclass = "btn btn-primary btn-block"
    def returnback: IRecord =
      if (isreturnback)
        parcel.getLogicalUri.map(x =>
          Record.data(PROP_REDIRECT -> x.toString)
        ).getOrElse(Record.empty)
      else
        Record.empty
    def submitbutton: Elem = 
      if (isactive)
        <input type="submit" value={submitname} class={buttonclass}/>
      else
        <input type="submit" value={submitname} class={buttonclass} disabled="true"/>
    def singleline = forminput
    def singlelineold: Elem = {
      def toinput(p: Parameter, ncol: Int, nvalue: Int): Seq[Elem] = {
        val id = generate_id()
        Vector(
          <div class={s"col-$ncol"}>
            <label class="col-form-label" for={id}>{p.takeLabel(locale)}</label>
          </div>,
          <div class={s"col-$nvalue"}>{
            // p.toInput(locale, id, "form-control")
            to_input(p.toColumn(strategy), id)
          }</div>
        )
      }
      <div class="form-group">
        <div class="form-row align-items-center"> {
          val (nl, nv) = parameters.length match {
            case 0 => RAISE.noReachDefect
            case 1 => (2, 8)
            case 2 => (2, 3)
            case 3 => (1, 2)
          }
          parameters.flatMap(toinput(_, nl, nv)) :+ (
            <div class="col-2">
              {submitbutton}
            </div>
          )
        } </div>
      </div>
    }
    def multiline = forminput
    def multilineold: Elem = {
      def toinput(p: Parameter): Elem = {
        val id = generate_id()
        <div class="form-row">
          <div class={s"col-4"}>
            <label class="col-form-label" for={id}>{p.label}</label>,
          </div>
          <div class={s"col-8"}>{
            // p.toInput(locale, id, "form-control")
            to_input(p.toColumn(strategy), id)
          }</div>
        </div>
      }
      <div class="form-group"> {
        List(
          parameters.flatMap(toinput),
          <div class="form-row">
            <div class="col-2">{
              submitbutton
            }</div>
          </div>
        )
      } </div>
    }
    def forminput = {
      val schema = Parameter.toSchema(parameters)(strategy)
      val hiddens = Hiddens(returnback)
      val button = Submit(ExecuteSubmitKind)
      val values = Record.empty
      val submits = Submits(button)
      val input = InputForm(
        new URI(action),
        Get,
        schema,
        values,
        hiddens,
        submits
      )
      property_input_form(input) match {
        case m: Elem => m
        case m => <div>{m}</div>
      }
    }
    val form = <div>{
      val descs: Seq[Elem] = description.flatMap(_.get(locale)).map(x =>
        <div class="row">
          <div class="col-auto">{_complement_p(x)}</div>
        </div>
      ).toList
      val params: Seq[Elem] = if (parameters.length > 3)
        List(multiline)
      else
        List(singleline)
      descs ++ params
    }</div>
    // val xs = returnback
    // val form = <form method={method.name} action={action}>{
    //   val descs: Seq[Elem] = description.flatMap(_.get(locale)).map(x =>
    //     <div class="row">
    //       <div class="col-auto">{_complement_p(x)}</div>
    //     </div>
    //   ).toList
    //   val hiddens: Seq[Elem] = xs.toStringVector.map {
    //     case (k, v) => <input type="hidden" name={k} value={v}></input>
    //   }
    //   val params: Seq[Elem] = if (parameters.length > 3)
    //     List(multiline)
    //   else
    //     List(singleline)
    //   descs ++ hiddens ++ params
    // }</form>
    val card = <div class="card">{
      val headers = title.flatMap(_.get(locale)).map(x =>
        <div class="card-header">
          <div class="card-title">
            <h5>{x}</h5>
          </div>
        </div>
      ).toList
      val bodys = List(
        <div class="card-body">
          <div class="container">
            {form}
          </div>
        </div>
      )
      headers ++ bodys
    }</div>
    card
  }

  protected def to_input(c: Column, id: String): Elem = property_input_form_item(c, id)

  private def _complement_p(p: NodeSeq): Node =
    p match {
      case m: Elem => m
      case _ => <p>{p}</p>
    }
}

object RendererFormPart {
}
