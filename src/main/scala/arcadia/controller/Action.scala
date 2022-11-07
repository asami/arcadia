package arcadia.controller

import scalaz._, Scalaz._
import java.net.{URL, URI}
import play.api.libs.json._
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.context.Conclusion
import org.goldenport.collection.NonEmptyVector
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => _, _}
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.json.JsonUtils
import org.goldenport.values.{Urn, PathName}
import org.goldenport.io.{UriBuilder, UriUtils}
import org.goldenport.trace.Result
import org.goldenport.value._
import org.goldenport.util.{StringUtils, SeqUtils, MapUtils}
import arcadia._
import arcadia.context._
import arcadia.model._
import arcadia.domain._
import arcadia.rule._
import arcadia.scenario.ScenarioEngine
import arcadia.scenario._

/*
 * @since   Jul. 16, 2017
 *  version Aug. 29, 2017
 *  version Sep. 21, 2017
 *  version Oct. 31, 2017
 *  version Nov. 13, 2017
 *  version Dec. 21, 2017
 *  version Jan. 22, 2018
 *  version Feb. 17, 2018
 *  version Mar. 26, 2018
 *  version Apr. 15, 2018
 *  version Jul. 16, 2018
 *  version Aug. 31, 2018
 *  version Sep.  5, 2018
 *  version Nov.  7, 2018
 *  version Apr. 30, 2019
 *  version May.  1, 2019
 *  version Mar. 31, 2020
 *  version Apr. 18, 2020
 *  version May. 28, 2020
 *  version Apr. 25, 2022
 *  version May.  3, 2022
 * @version Nov.  6, 2022
 * @author  ASAMI, Tomoharu
 */
trait Action {
  lazy val show: String = s"${getClass.getSimpleName}${show_info}"
  protected def show_info: String =
    show_Info match {
      case m if m.isEmpty => ""
      case m => 
        val a = m.map {
          case (k, v) => s"${k}=${v}"
        }.mkString(",")
        s"($a)"
    }

  protected def show_Info: Vector[(String, String)] = Vector.empty

  def apply(parcel: Parcel): Parcel = parcel.executeWithTrace(s"${show}#apply", parcel.show) {
    val r = execute_Apply(parcel)
    Result(r, r.show)
  }

  protected def execute_Apply(parcel: Parcel): Parcel

  def applyAjax(parcel: Parcel): Parcel = parcel.executeWithTrace(s"${show}#applyAjax", parcel.show) {
    val r = execute_Apply_Ajax(parcel)
    Result(r, r.show)
  }

  protected def execute_Apply_Ajax(parcel: Parcel): Parcel = parcel

  protected final def fetch_source_via_string[T](f: String => T)(parcel: Parcel, s: Source): Option[T] = {
    s match {
      case m: UrlSource => RAISE.notImplementedYetDefect
      case m: UriSource => RAISE.notImplementedYetDefect
      case m: UrnSource => parcel.fetchString(m).map(f)
      case m: BrokenSource => RAISE.notImplementedYetDefect
    }
  }

  protected final def fetch_source_via_string_option[T](f: String => Option[T])(parcel: Parcel, s: Source): Option[T] = {
    s match {
      case m: UrlSource => RAISE.notImplementedYetDefect
      case m: UriSource => RAISE.notImplementedYetDefect
      case m: UrnSource => parcel.fetchString(m).flatMap(f)
      case m: BrokenSource => RAISE.notImplementedYetDefect
    }
  }


  protected final def fetch_picture_list(parcel: Parcel, s: Source): List[Picture] =
    fetch_source_via_string(Picture.parseList)(parcel, s) getOrElse Nil

  protected final def fetch_badge(parcel: Parcel, s: Source): Option[Badge] =
    s match {
      case m: UrnSource => parcel.fetchBadge(m)
      case m: UrlSource => RAISE.notImplementedYetDefect
      case m: UriSource => RAISE.notImplementedYetDefect
      case m: BrokenSource => RAISE.notImplementedYetDefect
    }

  protected final def fetch_xml(parcel: Parcel, s: Source): Option[Xml] =
    fetch_source_via_string(Xml.parse)(parcel, s)

  protected final def fetch_xml_option(parcel: Parcel, s: Source): Option[Xml] =
    fetch_source_via_string_option(x =>
      if (Strings.notblankp(x)) None else Some(Xml.parse(x))
    )(parcel, s)

  protected final def fetch_request_parameter(parcel: Parcel, s: Source): Option[RequestParameter] =
    fetch_source_via_string(RequestParameter.parse)(parcel, s)

  protected final def fetch_schema(parcel: Parcel, s: Source): Option[Schema] =
    fetch_source_via_string(Schema.json.unmarshall)(parcel, s)

  protected final def fetch_columns(parcel: Parcel, s: Source): Option[Seq[FormColumn]] =
    fetch_source_via_string(FormColumn.parseList)(parcel, s)

  protected final def execute_pathname(parcel: Parcel)(body: PathName => Parcel): Parcel =
    parcel.command.map {
      case MaterialCommand(pathname) => body(pathname)
      case _ => parcel
    } getOrElse(parcel)

  protected final def is_valid(cond: Option[IRecord], target: IRecord): Boolean =
    cond.fold(true)(is_valid(_, target))

  protected final def is_valid(cond: IRecord, target: IRecord): Boolean = {
    // TODO sexpr for json
    cond.fields.map { field =>
      lazy val targetv = target.getString(field.key)
      field.asStringList match {
        case Nil => !target.isDefined(field.key)
        case x :: Nil => targetv.fold(false)(_ === x)
        case xs => targetv.fold(false)(x => xs.exists(_ === x)) // OR
      }
    }.forall(identity) // AND
  }

  protected final def get_domain_object_id(parcel: Parcel): Option[DomainObjectId] =
    parcel.getDomainObjectId

  protected final def take_domain_object_id(parcel: Parcel): DomainObjectId =
    get_domain_object_id(parcel) getOrElse {
      RAISE.illegalConfigurationDefect(s"${getClass.getSimpleName}: Missing id")
    }

  protected final def get_domain_entity_id(parcel: Parcel): Option[DomainEntityId] =
    parcel.getEffectiveModel.flatMap {
      case m: IRecordModel => m.getDomainEntityId
      case _ => None
    }

  protected final def take_domain_entity_id(parcel: Parcel): DomainEntityId =
    get_domain_entity_id(parcel) getOrElse {
      RAISE.illegalConfigurationDefect(s"${getClass.getSimpleName}: Missing id")
    }
}
object Action {
  import org.goldenport.json.JsonUtils.Implicits._
  import Schema.json._
  import Record.json._
  import Parameter.json._

  val currentPageFormAction = new URI("")

  implicit val MethodFormat = new JsonUtils.ValueFormat[Method](
    _.toLowerCase match {
      case "get" => Get
      case "post" => Post
      case "put" => Put
      case "delete" => Delete
    },
    _.name
  )
  implicit val SinkFormat = new Format[Sink] {
    def reads(json: JsValue): JsResult[Sink] = json match {
      case JsString(s) => JsSuccess(Sink(s))
      case _ => JsError(s"Unavailabel sink: $json")
    }
    def writes(p: Sink): JsValue = RAISE.notImplementedYetDefect
  }
  implicit val SourceFormat = new Format[Source] {
    def reads(json: JsValue): JsResult[Source] = json match {
      case JsString(s) => JsSuccess(Source(s))
      case _ => JsError(s"Unavailabel source: $json")
    }
    def writes(p: Source): JsValue = RAISE.notImplementedYetDefect
  }
  implicit val FormColumnFormat = Json.format[FormColumn]
  implicit val PropertyFormat = Json.format[Property]
  implicit val PropertyActionFormat = Json.format[PropertyAction]
  implicit val OperationActionFormat = Json.format[OperationAction]
  implicit val GetEntityActionFormat = Json.format[GetEntityAction]
  implicit val ReadEntityListActionFormat = Json.format[ReadEntityListAction]
  implicit val UpdateEntityDirectiveActionFormat = Json.format[UpdateEntityDirectiveAction]
  implicit val InvokeDirectiveActionFormat = Json.format[InvokeDirectiveAction]
  implicit val InvokeWithIdDirectiveActionFormat = Json.format[InvokeWithIdDirectiveAction]
//  implicit val ResetPasswordDirectiveActionFormat = Json.format[ResetPasswordDirectiveAction]
//  implicit val ResetPasswordOperationActionFormat = Json.format[ResetPasswordOperationAction]
  implicit val CarouselActionFormat = Json.format[CarouselAction]
  implicit val BannerActionFormat = Json.format[BannerAction]
  implicit val BadgeActionFormat = Json.format[BadgeAction]
  implicit val NoticeActionFormat = Json.format[NoticeAction]
  implicit val ContentActionFormat = Json.format[ContentAction]
  implicit val SearchBoxActionFormat = Json.format[SearchBoxAction]
  implicit val InvokeOperationScenarioActionFormat = Json.format[InvokeOperationScenarioAction]
  implicit val LoginScenarioActionFormat = Json.format[LoginScenarioAction]
  implicit val ResetPasswordScenarioActionFormat = Json.format[ResetPasswordScenarioAction]

  implicit object ActionReads extends Reads[Action] {
    def reads(json: JsValue): JsResult[Action] = parseJsValue(json)
  }

  def parseActionList(p: String): List[Action] = toActionList(Json.parse(p))

  def parseJsValue(json: JsValue): JsResult[Action] =
    json match {
      case m: JsObject => parseJsObject(m)
      case _ => JsError(s"Not js object")
    }

  def parseJsObject(json: JsObject): JsResult[Action] =
    (json \ "action").asOpt[String] match {
      case Some(s) => s match {
        // control
        case "property" => Json.fromJson[PropertyAction](json)
        // operation
        case "operation" => Json.fromJson[OperationAction](json)
        case "get-entity" => Json.fromJson[GetEntityAction](json)
        case "read-entity-list" => Json.fromJson[ReadEntityListAction](json)
        case "update-entity-directive" => Json.fromJson[UpdateEntityDirectiveAction](json)
        case "invoke-directive" => Json.fromJson[InvokeDirectiveAction](json)
        case "invoke-with-id-directive" => Json.fromJson[InvokeWithIdDirectiveAction](json)
//        case "reset-password-directive" => Json.fromJson[ResetPasswordDirectiveAction](json)
//        case "reset-password-operation" => Json.fromJson[ResetPasswordOperationAction](json)
        // widget
        case "carousel" => Json.fromJson[CarouselAction](json)
        case "banner" => Json.fromJson[BannerAction](json)
        case "badge" => Json.fromJson[BadgeAction](json)
        case "notice" => Json.fromJson[NoticeAction](json)
        case "content" => Json.fromJson[ContentAction](json)
        case "searchbox" => Json.fromJson[SearchBoxAction](json)
        // scenario
        case "invoke-operation-scenario" => Json.fromJson[InvokeOperationScenarioAction](json)
        case "login-scenario" => Json.fromJson[LoginScenarioAction](json)
        case "reset-password-scenario" => Json.fromJson[ResetPasswordScenarioAction](json)
        case _ => JsError(s"Unknown action '$s'")
      }
      case None => JsError(s"No action")
    }

  def parse(json: JsValue): Action =
    Json.fromJson[Action](json) match {
      case JsSuccess(s, _) => s
      case m: JsError => throw new IllegalArgumentException(m.toString)
    }

  def toAction(json: JsValue): Action = Json.fromJson[Action](json) match {
      case JsSuccess(s, _) => s
      case m: JsError => BrokenAction(m)
  }

  def toActionList(json: JsValue): List[Action] = json match {
    case JsArray(xs) => xs.toList.map(toAction)
    case m: JsObject => List(toAction(m))
    case m => RAISE.notImplementedYetDefect
  }
}

trait SourceSinkAction extends Action {
  def source: Option[Source]
  def sink: Option[Sink]

  override protected def show_Info = SeqUtils.buildTupleVector(
    "source" -> source.map(_.show),
    "sink" -> sink.map(_.show)
  )

  protected final def execute_source_sink(parcel: Parcel)(body: Source => Model): Parcel =
    source.fold(parcel) { src =>
      val r = body(src)
      set_sink(parcel, r)
    }

  protected final def set_sink(parcel: Parcel, model: Model): Parcel =
    sink.fold(
      parcel.withModel(model)
    )(sk =>
      parcel.sink(sk, model)
    )

  protected final def set_sink(parcel: Parcel, command: Command, model: Model): Parcel =
    sink.fold(
      parcel.withCommand(command).withModel(model)
    )(sk =>
      parcel.sink(sk, command, model)
    )
}

case class PropertyAction(
  properties: List[Property]
) extends Action {
  
  protected def execute_Apply(parcel: Parcel): Parcel =
    parcel.addProperties(properties)
}

object PropertyAction {
}

case class IndexAction(
) extends Action {
  import IndexAction._
  protected def execute_Apply(parcel: Parcel): Parcel =
    if (parcel.getEffectiveModel.isDefined) parcel else {
      val pagename = I18NString("Index page name") // TODO
      val headline = I18NElement("Index headline") // TODO
      val resources = parcel.context.map(TakeResources(_, parcel).apply) getOrElse Nil
      val carousel = None
      val model = IndexModel(
        Some(pagename),
        Some(headline),
        resources,
        carousel
      )
      parcel.withModel(model)
    }
}

object IndexAction {
  case class TakeResources(context: ExecutionContext, parcel: Parcel) extends ActionOperationBase {
    override val isDemo = false

    def apply: List[(String, EntityListModel)] = {
      if (isDemo)
        List(
          _read_entity_list_news,
          _read_entity_list_blog
        )
      else
        Nil
    }

    private def _read_entity_list_news() = {
      val rsc = DomainEntityType("news")
      val q = Query(rsc, 0, 10, 20)
      rsc.v -> context.readEntityList(q)
    }

    private def _read_entity_list_blog() = {
      val rsc = DomainEntityType("blog")
      val q = Query(rsc, 0, 10, 20)
      rsc.v -> context.readEntityList(q)
    }
  }
}

case class ResourceDetailAction(
) extends Action {
  protected def execute_Apply(parcel: Parcel): Parcel = execute_pathname(parcel) { pathname =>
    val a: Option[Parcel] = parcel.render.flatMap(_.viewContext.map(_.engine)).map { viewengine =>
      pathname.components match {
        case Nil => parcel
        case x :: Nil => parcel
        case x :: xx :: Nil =>
          val a = MaterialCommand(PathName(x, xx))
          def b = MaterialCommand(PathName(x))
          if (viewengine.findView(parcel.withCommand(a)).isDefined)
            parcel
          else if (viewengine.findView(parcel.withCommand(b)).isDefined)
            RAISE.notImplementedYetDefect
          else
            parcel
        case xs => RAISE.notImplementedYetDefect
      }
    }
    a.getOrElse(parcel)
  }
}

case class ScenarioAction(
  engine: ScenarioEngine
) extends Action {
  protected def execute_Apply(parcel: Parcel): Parcel = engine.apply(parcel)
}

case class OperationAction(
  operation: String,
  query: Option[Map[String, Any]],
  form: Option[Map[String, Any]],
  model: Option[String],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = parcel.applyOnContext { context =>
    def param = ModelParameter(model)
    val r = context.get(operation, query, form)
    Model.get(param, r).map(set_sink(parcel, _)).getOrElse {
      RAISE.noReachDefect
    }
  }
}

case class GetEntityAction(
  entity: String,
  id: Option[String],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  override protected def show_Info =
    SeqUtils.buildTupleVector(
      "entity" -> Some(entity),
      "id" -> id
    ) ++ super.show_Info

  protected def execute_Apply(parcel: Parcel): Parcel = parcel.applyOnContext { context =>
    (
      for {
        did <- id.map(StringDomainObjectId) orElse context.getIdInRequest
        r <- context.getEntity(DomainEntityType(entity), did)
      } yield parcel.withModel(r)
    ).getOrElse(parcel)
  }
}

case class ReadEntityListAction(
  entity: String,
  query: Option[Map[String, Any]],
  form: Option[Map[String, Any]],
  data_href: Option[URI],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  override protected def show_Info =
    SeqUtils.buildTupleVector(
      "entity" -> Some(entity),
      "query" -> query.map(x => s"query${MapUtils.show(x)}"),
      "form" -> form.map(x => s"form${MapUtils.show(x)}"),
      "data_href" -> data_href.map(_.toString)
    ) ++ super.show_Info

  protected def execute_Apply(parcel: Parcel): Parcel = parcel.applyOnContext { context =>
    val srcparams = source.flatMap(src =>
      fetch_request_parameter(parcel, src).flatMap(_.query)
    )
    val queryparams = parcel.inputQueryParameters
    // srcparams > queryparams (> srcparams) > query
    // val q = Query(
    //   DomainEntityType(entity),
    //   parameters = query.map(Record.create).getOrElse(Record.empty)
    val q = Query.create(entity, query).
      withParameter(queryparams).
      withParameter(srcparams)
    val r0 = context.readEntityList(q)
    val r = r0.withDataHref(data_href)
    set_sink(parcel, r)
  }

  override protected def execute_Apply_Ajax(parcel: Parcel): Parcel =
    RAISE.notImplementedYetDefect
}

case class UpdateEntityDirectiveAction(
  uri: URI,
  label: I18NString,
  condition: Option[IRecord],
  properties: Option[IRecord],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = {
    val id = take_domain_entity_id(parcel)
    val active = parcel.getEffectiveModel.map {
      case m: IRecordModel => is_valid(condition, m.record)
    }.getOrElse(false)
    val model = UpdateEntityDirectiveFormModel(
      uri,
      label,
      id,
      properties getOrElse Record.empty,
      active
    )
    set_sink(parcel, model)
  }
}
object UpdateEntityDirectiveAction {
}

case class InvokeDirectiveAction(
  uri: URI,
  method: Option[Method],
  title: Option[I18NElement],
  description: Option[I18NElement],
  submitLabel: Option[I18NElement],
  parameters: Parameters,
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = {
    val arguments = parcel.inputFormParameters // XXX use inputQueryParameters in GET
    val active = true
    val model = InvokeDirectiveFormModel(
      uri,
      method getOrElse Get,
      title,
      description,
      submitLabel,
      Parameters.resolve(parcel, parameters),
      arguments,
      active
    )
    set_sink(parcel, model)
  }
}

case class InvokeWithIdDirectiveAction(
  uri: URI,
  method: Option[Method],
  label: I18NString,
  condition: Option[IRecord],
  properties: Option[IRecord],
  idPropertyName: Option[String],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = {
    val id = take_domain_object_id(parcel)
    val active = parcel.getEffectiveModel.map {
      case m: IRecordModel => is_valid(condition, m.record)
    }.getOrElse(false)
    val model = InvokeWithIdDirectiveFormModel(
      uri,
      method getOrElse Post, // XXX Get
      label,
      id,
      properties getOrElse Record.empty,
      active,
      idPropertyName
    )
    set_sink(parcel, model)
  }
}
object InvokeWithIdDirectiveAction {
}

case class InvokeOperationScenarioAction(
  formAction: Option[URI],
  operation: URI,
  method: Option[Method],
  title: Option[I18NElement],
  description: Option[I18NElement],
  submitLabel: Option[I18NElement],
  parameters: Parameters,
  successView: Option[String],
  errorView: Option[String],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  def effectiveMethod: Method = method getOrElse Get

  protected def execute_Apply(parcel: Parcel): Parcel = {
    parcel.getPlatformContext.map { ctx =>
      val x = InvokeOperationScenario.launch(parcel, this)
      val rule = ScenarioEngine.Rule()
      val engine = new ScenarioEngine(ctx, rule)
      engine.apply(x)
    }.getOrElse(RAISE.noReachDefect)
  }

  // protected def execute_Apply(parcel: Parcel): Parcel = {
  //   val arguments = parcel.inputFormParameters // XXX use inputQueryParameters in GET
  //   val active = true
  //   val model = InvokeDirectiveFormModel(
  //     uri,
  //     method getOrElse Get,
  //     title,
  //     description,
  //     submitLabel,
  //     Parameters.resolve(parcel, parameters),
  //     arguments,
  //     active
  //   )
  //   set_sink(parcel, model)
  // }
}

case class LoginScenarioAction(
  formAction: Option[URI],
  title: Option[I18NElement],
  description: Option[I18NElement],
  usernameLabel: Option[I18NElement],
  passwordLabel: Option[I18NElement],
  submitLabel: Option[I18NElement],
  successRedirect: Option[String],
  successView: Option[String],
  errorView: Option[String],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = {
    parcel.getPlatformContext.map { ctx =>
      val x = LoginScenario.launch(parcel, this)
      val rule = ScenarioEngine.Rule()
      val engine = new ScenarioEngine(ctx, rule)
      engine.apply(x)
    }.getOrElse(RAISE.noReachDefect)
  }

  // protected def execute_Apply(parcel: Parcel): Parcel = {
  //   val arguments = parcel.inputFormParameters // XXX use inputQueryParameters in GET
  //   val active = true
  //   val model = InvokeDirectiveFormModel(
  //     uri,
  //     method getOrElse Get,
  //     title,
  //     description,
  //     submitLabel,
  //     Parameters.resolve(parcel, parameters),
  //     arguments,
  //     active
  //   )
  //   set_sink(parcel, model)
  // }
}

case class ResetPasswordScenarioAction(
  formAction: Option[URI],
  title: Option[I18NElement],
  description: Option[I18NElement],
  passwordLabel: Option[I18NElement],
  passwordConfirmLabel: Option[I18NElement],
  executeLabel: Option[I18NElement],
  successRedirect: Option[String],
  successView: Option[String],
  errorView: Option[String],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = {
    parcel.getPlatformContext.map { ctx =>
      val x = ResetPasswordScenario.launch(parcel, this)
      val rule = ScenarioEngine.Rule()
      val engine = new ScenarioEngine(ctx, rule)
      engine.apply(x)
    }.getOrElse(RAISE.noReachDefect)
  }
}

trait InteractiveOperationAction extends SourceSinkAction {
  import InteractiveOperationAction._

  protected final def operation_status(parcel: Parcel): Status =
    operation_Staus(parcel) getOrElse {
      val params = parcel.inputFormParameters
      params.getString(PROP_INTERACTIVE_OPERATION_STAUS).
        map(x => Status.get(x).getOrElse(RAISE.noReachDefect)).
        getOrElse(
          if (params.isEmpty)
            InitStatus
          else
            InputStatus
        )
    }

  protected def operation_Staus(parcel: Parcel): Option[Status] = None

  protected final def execute_Apply(parcel: Parcel): Parcel = {
    operation_status(parcel) match {
      case InitStatus => execute_Init(parcel)
      case InputStatus => execute_Input(parcel)
      case ConfirmStatus => execute_Confirm(parcel)
      // case DoneStatus => execute_Done(parcel)
      // case ErrorStatus => execute_Error(parcel)
    }
  }

  protected def execute_init(parcel: Parcel): Parcel = {
    execute_Init(parcel)
  }

  protected def execute_input(parcel: Parcel): Parcel = {
    execute_Input(parcel)
  }

  protected def execute_confirm(parcel: Parcel): Parcel = {
    execute_Confirm(parcel)
  }

  protected def execute_Init(parcel: Parcel): Parcel
  protected def execute_Input(parcel: Parcel): Parcel
  protected def execute_Confirm(parcel: Parcel): Parcel = RAISE.unsupportedOperationFault
  // protected def execute_Done(parcel: Parcel): Parcel
  // protected def execute_Error(parcel: Parcel): Parcel
}
object InteractiveOperationAction {
  val PROP_INTERACTIVE_OPERATION_STAUS = "_interactive_operation_status"

  sealed trait Status extends NamedValueInstance {
  }
  object Status extends EnumerationClass[Status] {
    val elements = Vector(InitStatus, InputStatus, ConfirmStatus)
  }
  case object InitStatus extends Status {
    val name = "init"
  }
  case object InputStatus extends Status {
    val name = "input"
  }
  case object ConfirmStatus extends Status {
    val name = "confirm"
  }
  // case object DoneStatus extends Status {
  // }
  // case object ErrorStatus extends Status {
  // }
  // val STATUS_INIT = "init"
  // val STATUS_INPUT = "input"
  // val STATUS_CONFIRM = "confirm"
  // val STATUS_DONE = "done"
  // val STATUS_ERROR = "error"
}

case class ResetPasswordOperationAction(
  okLabel: Option[I18NElement],
  source: Option[Source],
  sink: Option[Sink]
) extends InteractiveOperationAction {
  import InteractiveOperationAction._

  def inputView: String = "reset"
  def confirmView: Option[String] = None // "confirm"
  def doneView: String = "complete"
  def errorView: String = "error"

  protected def make_Model(parcel: Parcel) = {
    val rule = parcel.context.map(_.resetPasswordRule) getOrElse ResetPasswordRule.default
    rule.toDirectiveModel(new URI(""), okLabel, parcel.inputFormParameters)
  }

  protected def make_Model(parcel: Parcel, p: Invalid) = {
    val rule = parcel.context.map(_.resetPasswordRule) getOrElse ResetPasswordRule.default
    rule.toDirectiveModel(new URI(""), okLabel, parcel.inputFormParameters, FormModel.Conclusion(p))
  }

  protected def execute_Init(parcel: Parcel): Parcel = {
    val model = make_Model(parcel)
    val command = ViewCommand(inputView)
    set_sink(parcel, command, model)
  }

  protected def execute_Input(parcel: Parcel): Parcel = {
    implicit val strategyctx = parcel.toStrategy
    val rule = parcel.context.map(_.resetPasswordRule) getOrElse ResetPasswordRule.default
    val parameters = Parameters.resolve(parcel, rule.toParameters)
    val input = parcel.inputFormParameters
    val v = parameters.validate(input)(strategyctx)
    v match {
      case Valid => _do(parcel, input)
      case m: Warning => _do(parcel, input)
      case m: Invalid => _rerun(parcel, m)
    }
  }

  private def _rerun(parcel: Parcel, p: Invalid) = {
    val model = make_Model(parcel, p)
    val command = ViewCommand(inputView)
    set_sink(parcel, command, model)
  }

  private def _do(parcel: Parcel, input: IRecord) = parcel.applyOnContext { ctx =>
    val res = ctx.post("resetpassword_complete", input) // TODO customizable
    if (res.isSuccess)
      _done(parcel, res)
    else
      _error(parcel, res)
  }

  private def _done(parcel: Parcel, res: Response) = {
    val command = ViewCommand(doneView)
    val model = res.toModel
    parcel.withCommand(command).withModel(model)
  }

  private def _error(parcel: Parcel, res: Response) = {
    val command = ViewCommand(errorView)
    val model = ErrorModel.create(res)
    parcel.withCommand(command).withModel(model)
  }
}

// case class ResetPasswordDirectiveAction(
//   uri: URI,
//   okLabel: Option[I18NElement],
//   source: Option[Source],
//   sink: Option[Sink]
// ) extends SourceSinkAction {
//   protected def execute_Apply(parcel: Parcel): Parcel = {
//     val rule = parcel.context.map(_.resetPasswordRule) getOrElse ResetPasswordRule.default
//     val targeturi = UriUtils.sibling(uri)
//     val model = rule.toDirectiveModel(parcel.locale, targeturi, okLabel)
//     set_sink(parcel)(model)
//   }
// }

// case class ResetPasswordOperationAction(
//   uri: URI,
//   okLabel: Option[I18NElement],
//   source: Option[Source],
//   sink: Option[Sink]
// ) extends SourceSinkAction {
//   import org.goldenport.record.v2.{Valid, Invalid}

//   protected def execute_Apply(parcel: Parcel): Parcel = {
//     implicit val strategyctx = parcel.toStrategy
//     val rule = parcel.context.map(_.resetPasswordRule) getOrElse ResetPasswordRule.default
//     val parameters = Parameters.resolve(parcel, rule.toParameters(parcel.locale))
//     val input = parcel.inputFormParameters
//     val v = parameters.validate(input)(strategyctx)
//     v match {
//       case Valid => _do(parcel, input)
//       case m: Warning => _do(parcel, input)
//       case m: Invalid => _rerun(parcel, m)
//     }
//   }

//   private def _do(parcel: Parcel, input: IRecord) = parcel.applyOnContext { ctx =>
//     val res = ctx.post("resetpassword_complete", input)
//     _go(parcel)
//   }

//   private def _go(parcel: Parcel) = {
//     parcel // TODO
//   }

//   private def _rerun(parcel: Parcel, p: Invalid) = {
//     val model = 
//     parcel.

//     ???
//   }
// }

case class CarouselAction(
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
    val a = fetch_picture_list(parcel, src)
    CarouselModel(a)
  }
}

case class BannerAction(
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
    val a = fetch_picture_list(parcel, src)
    BannerModel(a)
  }
}

case class BadgeAction(
  entity: Option[String],
  query: Option[Map[String, Any]],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
    val a = fetch_badge(parcel, src) getOrElse Badge.empty
    BadgeModel(a)
  }
}

case class NoticeAction(
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
    val a = fetch_xml_option(parcel, src)
    NoticeModel(a)
  }
}

case class ContentAction(
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
    val a = fetch_xml(parcel, src) getOrElse Xml.empty
    XmlModel(a)
  }
}

case class SearchBoxAction(
  action: Option[URI],
  columns: Option[Seq[FormColumn]],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  protected def execute_Apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
    val cs: Seq[FormColumn] = fetch_columns(parcel, src) orElse columns getOrElse {
      List(FormColumn.create("keywords", "search", "1", "検索"))
    }
    val a = cs.map { c =>
      Column(
        c.name,
        c.datatype.map(_datatype) getOrElse _datatype_by_property(parcel, c.name),
        c.multiplicity.map(_multiplicity) getOrElse MZeroOne,
        label = c.label,
        form = Column.Form(c.placeholder.map(I18NString(_)))
      )
    }
    val formaction = action getOrElse Action.currentPageFormAction
    val schema = Schema(a)
    val searchbox = arcadia.view.Renderer.SearchBox(formaction, schema)
    SearchBoxModel(searchbox)
  }

  private def _datatype(name: String): DataType =
    DataType.get(name) getOrElse XString
  private def _datatype_by_property(parcel: Parcel, name: String): DataType = parcel.execute { ctx =>
    ctx.getDefaultPropertyColumn(name).map(_.datatype) getOrElse XString
  }
  private def _multiplicity(p: String): Multiplicity = Multiplicity.to(p)
}

case class LoginAction(
) extends Action {
  protected def execute_Apply(parcel: Parcel): Parcel = parcel.withContent(RedirectContent())
}

case class LogoutAction(
) extends Action {
  protected def execute_Apply(parcel: Parcel): Parcel = parcel.withContent(RedirectContent())
}

case class RouterAction(
  route: Route
) extends Action {
  protected def execute_Apply(parcel: Parcel): Parcel = route.apply(parcel)
}

case class RedirectAction(
  page: String
) extends Action {
  protected def execute_Apply(parcel: Parcel): Parcel =
    parcel.withContent(RedirectContent(page))
}

case class RedirectSinglePageAction(
  page: String = "index.html"
) extends Action {
  protected def execute_Apply(parcel: Parcel): Parcel = {
    def uri(p: URI) = parcel.getEffectiveModel.fold(p) {
      case m: OperationOutcomeModel =>
        val builder = UriBuilder(p)
        val a = builder.addPath(page).addQuery(m.request.query.toRecord.asNameStringVector)
        a.build
      case _ => p
    }
    val a = parcel.render.flatMap(_.applicationRule.singlePageApplication.flatMap(_.base_uri.headOption))
    a.map(x => parcel.withContent(RedirectContent(uri(x)))).
      getOrElse(parcel)
  }

  // // TODO migrate org.goldenport.io.UriBuilder
  // def addQuery(base: Option[String], q: Record): Option[String] =
  //   base.map(b =>
  //     if (q.isEmpty)
  //       Some(b)
  //     else
  //       Some(s"${b}&${makeUrlQueryParams(q)}")
  //   ).getOrElse(
  //     Some(makeUrlQueryParams(q))
  //   )

  // // See StringUtils.addUrlParams
  // def makeUrlQueryParams(q: Record) = q.toStringVector.map {
  //   case (k, v) => s"${k}=${v}"
  // }.mkString("&")
}

case class InvokePlatformAction(
) extends Action {
  protected def execute_Apply(parcel: Parcel): Parcel = {
    parcel.command.collect {
      case m: InvokePlatformCommand => m
    }.flatMap(cmd =>
      parcel.context.map { ctx =>
        val res = ctx.invoke(cmd)
        val param = ModelParameter(None)
        Model.get(param, res).map(parcel.withModel).getOrElse(parcel)
      }
    ).getOrElse(parcel)
  }
}

case class InvokeOperationAction(
) extends Action {
  protected def execute_Apply(parcel: Parcel): Parcel = {
    parcel.command.collect {
      case m: InvokeOperationCommand => m
    }.flatMap(cmd =>
      parcel.context.map { ctx =>
        val res = ctx.invoke(cmd)
        val param = ModelParameter(None)
        Model.get(param, res).map(parcel.withModel).getOrElse(parcel)
      }
    ).getOrElse(parcel)
  }
}

case class BrokenAction(
  message: I18NString,
  json: Option[JsValue],
  jsonError: Option[JsError]
) extends Action {
  protected def execute_Apply(parcel: Parcel): Parcel = {
    val c = Conclusion.config.illegalConfigurationDefect(message.en)
    val model = ErrorModel.create(parcel, c)
    val command = ErrorCommand(model)
    parcel.withCommandModel(command, model)
  }
}
object BrokenAction {
  def apply(msg: String, json: JsValue): BrokenAction = BrokenAction(I18NString(msg), Some(json), None)
  def apply(p: JsError): BrokenAction = BrokenAction(JsonUtils.messageI18N(p), None, Some(p))
}

sealed trait Source {
  def show: String
}
object Source {
  def apply(p: String): Source = {
    val uri = new URI(p)
    Option(uri.getScheme).map {
      case "http" => UrlSource(uri.toURL)
      case "https" => UrlSource(uri.toURL)
      case "urn" => UrnSource(Urn(p))
      case _ => UriSource(uri)
    }.getOrElse(BrokenSource(p))
  }
}
case class UrlSource(url: URL) extends Source {
  def show = StringUtils.shortUrl(url)
}
case class UriSource(uri: URI) extends Source {
  def show = StringUtils.shortUri(uri)
}
case class UrnSource(urn: Urn) extends Source {
  def nid = urn.nid
  def module = urn.module
  def submodule = urn.submodule

  def show = StringUtils.shortUrn(urn)
}
object UrnSource {
  def apply(p: String): UrnSource = UrnSource(Urn(p))
}
case class BrokenSource(v: String) extends Source {
  def show = v
}

sealed trait Sink {
  def show: String
}
object Sink {
  def apply(p: String): Sink = ModelHangerSink(p)
}
case class ModelHangerSink(key: String) extends Sink {
  def show = key
}
