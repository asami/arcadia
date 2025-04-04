package arcadia.view.tag

import scalaz.{Node => _, _}, Scalaz._
import scala.xml._
import java.net.URI
import com.github.nscala_time.time.Imports._
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.collection.NonEmptyVector
import org.goldenport.xml.XmlUtils
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Schema, Column, Invalid, Warning}
import org.goldenport.i18n.{I18NElement, I18NString}
import org.goldenport.trace.Result
import org.goldenport.values.PathName
import org.goldenport.util.StringUtils
import arcadia._
import arcadia.context._
import arcadia.view._
import arcadia.model._
import arcadia.controller.Action
import arcadia.controller.Controller.PROP_REDIRECT

/*
 * @since   Oct. 11, 2017
 *  version Nov. 15, 2017
 *  version Dec. 13, 2017
 *  version Jan. 22, 2018
 *  version Feb. 17, 2018
 *  version Apr. 15, 2018
 *  version May.  6, 2018
 *  version Sep.  1, 2018
 *  version Nov.  7, 2018
 *  version Apr. 28, 2019
 *  version May.  1, 2019
 *  version Mar. 21, 2020
 *  version Apr. 18, 2020
 *  version Feb. 27, 2022
 *  version Mar. 30, 2022
 *  version May.  4, 2022
 *  version Mar. 28, 2025
 * @version Apr.  2, 2025
 * @author  ASAMI, Tomoharu
 */
trait Tag {
  lazy val show: String = s"${getClass.getSimpleName}${show_info}"
  protected def show_info: String =
    if (Strings.blankp(show_Info))
      ""
    else
      s"($show_Info)"
  protected def show_Info: String = ""

  final def eval(p: Expression): Option[XmlContent] =
    execute_Eval(p)
    // p.parcel.executeWithTraceOption(s"${show}#eval", p.parcel.show) {
    //   val r = execute_Eval(p)
    //   r.map(x => Result(x, x.show))
    // }
  protected def execute_Eval(p: Expression): Option[XmlContent]

  protected final def resolve_action(expr: Expression, p: URI): String =
    expr.resolveActionPathName(p).v

  protected final def command_submit_label(expr: Expression, p: Option[I18NElement]): String = {
    val a = expr.get("submitLabel").map(I18NElement.apply).getOrElse(
      expr.parcel.render.fold(I18NElement("Execute"))(_.label.commandSubmit)
    )
    expr.format(a)
  }
}

trait SelectByName { self: Tag =>
  def name: String
  protected def execute_Eval(p: Expression): Option[XmlContent] =
    if (p.isLabel(name))
      Some(eval_Expression(p))
    else
      None

  protected def eval_Expression(p: Expression): XmlContent
}

case object ErrorTag extends Tag with SelectByName {
  val name = "error"

  protected def eval_Expression(p: Expression): XmlContent = {
    p.applyModel
  }
}

case object ModelTag extends Tag with SelectByName {
  val name = "model"

  protected def eval_Expression(p: Expression): XmlContent = {
    p.applyModel
  }
}

case object ValueTag extends Tag with SelectByName {
  val name = "value"

  protected def eval_Expression(p: Expression): XmlContent = {
    val name = p.get("name")
    val v = p.service.get(name)
    XmlContent.text(v.print)
  }
}

case object WidgetTag extends Tag with SelectByName {
  val name = "widget"

  protected def eval_Expression(p: Expression): XmlContent = {
    val name = p.take("name")
    val model = WidgetModel(name, p)
    p.applyModel(model)
  }
}

case object TableTag extends Tag with SelectByName {
  val name = "table"

  protected def eval_Expression(p: Expression): XmlContent = {
    val caption = p.get("caption")
    val column = p.getStringList("column")
    val kind = p.get("kind")
    val model = p.effectiveModel match {
      case m: ITableModel => m.withCaptionSchemaKind(caption, column, kind)
      case m => m
    }
    p.applyModel(model)
  }
}

case object GridTag extends Tag with SelectByName {
  val name = "grid"

  protected def eval_Expression(p: Expression): XmlContent = {
    val column = p.getStringList("column")
    val card = p.get("card")
    val model = p.effectiveModel match {
      case m: ITableModel => m.withSchemaKind(column, GridTable)
      case m => m
    }
    p.withTable(GridTable).withCard(card).applyModel(model)
  }
}

case object ListTag extends Tag with SelectByName {
  val name = "list"

  protected def eval_Expression(p: Expression): XmlContent = {
    val column = p.getStringList("column")
    val card = p.get("card")
    val model = p.effectiveModel match {
      case m: ITableModel => m.withSchemaKind(column, ListTable)
      case m => m
    }
    p.withTable(ListTable).withCard(card).applyModel(model)
  }
}

case object DetailTag extends Tag with SelectByName {
  val name = "detail"

  protected def eval_Expression(p: Expression): XmlContent = {
    // val column = p.getStringList("column")
    // val card = p.get("card")
    val model = p.effectiveModel match {
      case m: EntityDetailModel => m // .withSchemaKind(column, GridTable)
      case m => m
    }
//    p.withTable(GridTable).withCard(card).applyModel(model)
    p.applyModel(model)
  }
}

case object SearchBoxTag extends Tag with SelectByName {
  val name = "searchbox"
  // keywords, created_at, updated_at, publish_at,
  // public_at, close_at, start_at, end_at
  protected def eval_Expression(p: Expression): XmlContent = {
    def create = p.parcel.execute { context =>
      val defaults = Vector("keywords")
      val columns = p.getStringList("column").map {
        case Nil => defaults
        case xs => xs // TODO parse column
      }.getOrElse {
        defaults
      }
      val formaction = Action.currentPageFormAction
      val schema = {
        val a = columns.map(x =>
          context.
            getDefaultPropertyColumn(x).
            getOrElse(Column(x))
        )
        Schema(a)
      }
      SearchBoxModel(formaction, schema)
    }
    val model = p.effectiveModel match {
      case m: SearchBoxModel => create
      case m: EntityListModel => create
    }
    p.applyModel(model)
  }
}

case object ContentTag extends Tag with SelectByName {
  val name = "content"

  protected def eval_Expression(p: Expression): XmlContent = {
    XmlContent(p.viewModel.content(p.getLayoutKind))
  }
}

case object BannerTag extends Tag with SelectByName {
  val name = "banner"

  protected def eval_Expression(p: Expression): XmlContent = {
    val model = p.effectiveModel
    p.applyModel(model)
  }
}

case object CarouselTag extends Tag with SelectByName {
  val name = "carousel"

  protected def eval_Expression(p: Expression): XmlContent = {
    val model = p.effectiveModel
    p.applyModel(model)
  }
}

case object BadgeTag extends Tag with SelectByName {
  val name = "badge"

  protected def eval_Expression(p: Expression): XmlContent = {
    val model = p.effectiveModel
    p.applyModel(model)
  }
}

case object NoticeTag extends Tag with SelectByName {
  val name = "notice"

  protected def eval_Expression(p: Expression): XmlContent = {
    val model = p.effectiveModel
    p.applyModel(model)
  }
}

case object ButtonTag extends Tag with SelectByName {
  val name = "button"

  protected def eval_Expression(p: Expression): XmlContent = {
    p.effectiveModel match {
//      case m: PropertyConfirmFormModel => _button(m)
      case m: UpdateEntityDirectiveFormModel => _button(p, m)
      case m: InvokeWithIdDirectiveFormModel => _button(p, m)
      case EmptyModel => RAISE.illegalConfigurationDefect(s"Empty model in Button(${p.show})")
      case m => RAISE.illegalConfigurationDefect(s"Unavailabel model in Button(${p.show}}: $m")
    }
  }

  // private def _button(p: PropertyConfirmFormModel) = {
  //   val action: String = ???
  //   val buttonname: String = ???
  //   val properties: String = ???
  //   val buttonclass = "btn btn-primary btn-round"
  //   val a = <form method="POST" action={action}>
  //     <input type="submit" value={buttonname} class={buttonclass}/>
  //   </form>
  //   XmlContent(a)
  // }

  private def _button(expr: Expression, p: UpdateEntityDirectiveFormModel): XmlContent = {
    val op = resolve_action(expr, p.action)
    val id = p.id.v
    val action: String = s"$op/$id"
    val buttonname: String = expr.format(p.label)
    val properties: IRecord = p.properties
    _button(expr, Put, action, buttonname, properties, p.isActive, true)
  }

  private def _button(expr: Expression, p: InvokeWithIdDirectiveFormModel): XmlContent = {
    val op = resolve_action(expr, p.action)
    val id = p.id.v
    val idname = p.idPropertyName getOrElse "id"
    val action: String = s"$op?$idname=$id"
    val buttonname: String = expr.format(p.label)
    val properties: IRecord = p.properties
    _button(expr, p.method, action, buttonname, properties, p.isActive, true)
  }

  private def _button(
    expr: Expression,
    method: Method,
    action: String,
    buttonname: String,
    properties: IRecord,
    isactive: Boolean,
    isreturnback: Boolean
  ): XmlContent = {
    def returnback: IRecord =
      if (isreturnback)
        expr.parcel.getLogicalUri.map(x =>
          Record.data(PROP_REDIRECT -> x.toString)
        ).getOrElse(Record.empty)
      else
        Record.empty
    val xs = properties + returnback
    // TODO renderer
    val buttonclass = "btn btn-primary btn-block"
    val r = <form method={method.name} action={action}> {
      xs.asNameStringVector.map {
        case (k, v) => <input type="hidden" name={k} value={v}></input>
      } :+ (
        if (isactive)
          <input type="submit" value={buttonname} class={buttonclass}/>
        else
          <input type="submit" value={buttonname} class={buttonclass} disabled="true"/>
      )
    } </form>
    XmlContent(r)
  }
}

case object CommandTag extends Tag with SelectByName {
  val name = "command"

  protected def eval_Expression(p: Expression): XmlContent = {
    p.effectiveModel match {
      case m: InvokeDirectiveFormModel => _command(p, m)
      case EmptyModel => RAISE.illegalConfigurationDefect(s"Empty model in Command(${p.show})")
      case m => RAISE.illegalConfigurationDefect(s"Unavailabel model in Command(${p.show}}: $m")
    }
  }

  private def _command(expr: Expression, p: InvokeDirectiveFormModel): XmlContent = {
    val op = resolve_action(expr, p.action)
    val action: String = s"$op"
    val method = p.method
    val title = p.title
    val description = p.description
    val submitlabel: String = command_submit_label(expr, p.submitLabel)
    val parameters = p.parameters
    val arguments = p.arguments
    val warnings = p.conclusion.getWarnings
    val errors = p.conclusion.getErrors
    _command(expr, method, action, title, description, submitlabel, parameters, arguments, p.isActive, false, warnings, errors)
  }

  private def _command(
    expr: Expression,
    method: Method,
    action: String,
    title: Option[I18NElement],
    description: Option[I18NElement],
    submitlabel: String,
    parameters: Parameters,
    arguments: IRecord,
    isactive: Boolean,
    isreturnback: Boolean,
    warnings: Option[NonEmptyVector[I18NString]],
    errors: Option[NonEmptyVector[I18NString]]
  ): XmlContent = {
    val strategy = expr.strategy
    val r = new Renderer(strategy) {
      protected def render_Content: NodeSeq = command_form(
        expr.parcel,
        method,
        action,
        title,
        description,
        submitlabel,
        parameters,
        arguments,
        isactive,
        isreturnback,
        warnings,
        errors
      )
    }.apply
    XmlContent(r)
  }
}

case object TabsTag extends Tag with SelectByName {
  val TAB_PANE = "tab-pane"
  val name = "tabs"

  protected def eval_Expression(p: Expression): XmlContent = {
    val a = p.children.flatMap(_.xml collect {
      case m: Elem if m.label == TAB_PANE => TabPane(m)
    })
    val b = Tabs(a.toList)
    val r = new Renderer(p.strategy) {
      protected def render_Content: NodeSeq = tabs(b)
    }.apply
    XmlContent(r)
  }
}

case object FormTag extends Tag with SelectByName {
  val name = "form"

  protected def eval_Expression(p: Expression): XmlContent = {
    val model = p.effectiveModel match {
      case m: FormModel => m
    }
    p.applyModel(model)
  }
}

case object DateTimeTag extends Tag with SelectByName {
  val name = "datetime"

  protected def eval_Expression(p: Expression): XmlContent = {
    val dt = p.service.dateTime
    XmlContent.text(dt.print)
  }
}

case object DateTag extends Tag with SelectByName {
  val name = "date"

  protected def eval_Expression(p: Expression): XmlContent = {
    val dt = p.service.date
    XmlContent.text(dt.print)
  }
}

case object TimeTag extends Tag with SelectByName {
  val name = "time"

  protected def eval_Expression(p: Expression): XmlContent = {
    val dt = p.service.time
    XmlContent.text(dt.print)
  }
}

/*
 * Layout Tags
 */
case object HeadDefTag extends Tag with SelectByName {
  val name = "head-def"

  protected def eval_Expression(p: Expression): XmlContent = {
    XmlContent(p.viewModel.headDef(p.getLayoutKind))
  }
}

case object FootDefTag extends Tag with SelectByName {
  val name = "foot-def"

  protected def eval_Expression(p: Expression): XmlContent = {
    XmlContent(p.viewModel.footDef(p.getLayoutKind))
  }
}

case object HeaderTag extends Tag with SelectByName {
  val name = "header"

  protected def eval_Expression(p: Expression): XmlContent = {
    XmlContent(p.viewModel.header(p.getLayoutKind))
  }
}

case object FooterTag extends Tag with SelectByName {
  val name = "footer"

  protected def eval_Expression(p: Expression): XmlContent = {
    XmlContent(p.viewModel.footer(p.getLayoutKind))
  }
}

case object SidebarTag extends Tag with SelectByName {
  val name = "sidebar"

  protected def eval_Expression(p: Expression): XmlContent = {
    XmlContent(p.viewModel.sidebar(p.getLayoutKind))
  }
}

case object NavigationTag extends Tag with SelectByName {
  val name = "navigation"

  protected def eval_Expression(p: Expression): XmlContent = {
    XmlContent(p.viewModel.navigation(p.getLayoutKind))
  }
}

case object ContentHeaderTag extends Tag with SelectByName {
  val name = "content-header"

  protected def eval_Expression(p: Expression): XmlContent = {
    XmlContent(p.viewModel.contentHeader(p.getLayoutKind))
  }
}

case object ContentMainTag extends Tag with SelectByName {
  val name = "content-main"

  protected def eval_Expression(p: Expression): XmlContent = {
    XmlContent(p.viewModel.contentContent)
  }
}

case object PageTitleTag extends Tag with SelectByName {
  val name = "page-title"

  protected def eval_Expression(p: Expression): XmlContent = {
    XmlContent(p.viewModel.pageTitle)
  }
}

case object LinkTag extends Tag with SelectByName {
  val name = "link"

  protected def eval_Expression(p: Expression): XmlContent = {
    val assets = p.viewModel.assets
    val path = p.take("path", "Missing path")
    val src = StringUtils.concatPath(assets, path)
    val rel = p.get("rel") getOrElse ""
    val xml = <link href={src} type={rel}/>
    XmlContent(xml)
  }
}

case object ScriptTag extends Tag with SelectByName {
  val name = "script"

  protected def eval_Expression(p: Expression): XmlContent = {
    val assets = p.viewModel.assets
    val path = p.take("path", "Missing path")
    val src = StringUtils.concatPath(assets, path)
    val t = p.get("type") getOrElse ""
    val xml = <script src={src} type={t}/>
    XmlContent(xml)
  }
}
