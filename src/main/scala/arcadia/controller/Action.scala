package arcadia.controller

import java.net.{URL, URI}
import play.api.libs.json._
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.json.JsonUtils
import org.goldenport.values.Urn
import arcadia._
import arcadia.context._
import arcadia.model._
import arcadia.domain._
import arcadia.scenario.ScenarioEngine

/*
 * @since   Jul. 16, 2017
 *  version Aug. 29, 2017
 *  version Sep. 21, 2017
 * @version Oct. 31, 2017
 * @author  ASAMI, Tomoharu
 */
trait Action {
  def apply(parcel: Parcel): Parcel

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
}
object Action {
  import org.goldenport.json.JsonUtils.Implicits._

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
  implicit val OperationActionFormat = Json.format[OperationAction]
  implicit val GetEntityActionFormat = Json.format[GetEntityAction]
  implicit val ReadEntityListActionFormat = Json.format[ReadEntityListAction]
  implicit val CarouselActionFormat = Json.format[CarouselAction]
  implicit val BannerActionFormat = Json.format[BannerAction]
  implicit val BadgeActionFormat = Json.format[BadgeAction]
  implicit val NoticeActionFormat = Json.format[NoticeAction]
  implicit val ContentActionFormat = Json.format[ContentAction]

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
        case "operation" => Json.fromJson[OperationAction](json)
        case "get-entity" => Json.fromJson[GetEntityAction](json)
        case "read-entity-list" => Json.fromJson[ReadEntityListAction](json)
        case "carousel" => Json.fromJson[CarouselAction](json)
        case "banner" => Json.fromJson[BannerAction](json)
        case "badge" => Json.fromJson[BadgeAction](json)
        case "notice" => Json.fromJson[NoticeAction](json)
        case "content" => Json.fromJson[ContentAction](json)
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

  protected final def execute_source_sink(parcel: Parcel)(body: Source => Model): Parcel =
    source.fold(parcel) { src =>
      val r = body(src)
      set_sink(parcel)(r)
    }

  protected final def set_sink(parcel: Parcel)(model: Model): Parcel =
    sink.fold(
      parcel.withModel(model)
    )(sk =>
      parcel.sink(sk, model)
    )
}

case class IndexAction(
) extends Action {
  import IndexAction._
  def apply(parcel: Parcel): Parcel =
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
      List(
        _read_entity_list_news,
        _read_entity_list_blog
      )
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

//     private def _read_resource_grid(c: EverforthClassKind) = {
//       val q = QueryContext.create(c, 0, 10, 20) // TODO
//       val m = resource_list_model(c, q)
//       (c.resourceName, m)
//     }

//     private def _read_resource_grid_demo(c: EverforthClassKind) = {
// //      val q = QueryContext.create(c, 0, 10, 20).withBrands(Vector(252)) // TODO
//       val q = QueryContext.create(c, 0, 10, 20).withIdsBag(Vector(
//         "pal-palshop-1377836961310-apparelcloud.blog-742c614e-bedd-4a18-ace3-447433b93f9f",
//         "pal-palshop-1410983251862-apparelcloud.blog-1e1b8dcf-d662-4598-bd6d-d5ce72500755",
//         "pal-palshop-1378000449352-apparelcloud.blog-37e2f626-2090-4f0f-ba76-535493d1d310",
//         "pal-palshop-1377947431680-apparelcloud.blog-7a93aa55-e683-4dae-b15a-38a376f7cca2",
//         "pal-palshop-1377946471327-apparelcloud.blog-d6cc560e-f0a4-429b-aaaa-245a3614a206"
//       )) // TODO
//       val m = resource_list_model(c, q)
//       (c.resourceName, m)
//     }
  }
}

case class ScenarioAction(
  engine: ScenarioEngine
) extends Action {
  def apply(parcel: Parcel): Parcel = engine.apply(parcel)
}

case class OperationAction(
  operation: String,
  query: Option[Map[String, Any]],
  form: Option[Map[String, Any]],
  model: Option[String],
  source: Option[Source],
  sink: Option[Sink]
) extends Action {
  def apply(parcel: Parcel): Parcel = parcel.applyOnContext { context =>
    def param = ModelParameter(model)
    val r = context.get(operation, query, form)
    Model.get(param, r).map(parcel.withModel(_)).getOrElse {
      RAISE.noReachDefect
    }
  }
}

case class GetEntityAction(
  entityType: String,
  id: String,
  source: Option[Source],
  sink: Option[Sink]
) extends Action {
  def apply(parcel: Parcel): Parcel = parcel.applyOnContext { context =>
    val r = context.getEntity(DomainEntityType(entityType), StringDomainObjectId(id))
    r.fold(parcel)(parcel.withModel)
  }
}

case class ReadEntityListAction(
  entity: String,
  query: Option[Map[String, Any]],
  form: Option[Map[String, Any]],
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  def apply(parcel: Parcel): Parcel = parcel.applyOnContext { context =>
    val q = Query(DomainEntityType(entity), parameters = query.getOrElse(Map.empty))
    val r = context.readEntityList(q)
    set_sink(parcel)(r)
  }
}

case class CarouselAction(
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  def apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
    val a = fetch_picture_list(parcel, src)
    CarouselModel(a)
  }
}

case class BannerAction(
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  def apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
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
  def apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
    val a = fetch_badge(parcel, src) getOrElse Badge.empty
    BadgeModel(a)
  }
}

case class NoticeAction(
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  def apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
    val a = fetch_xml_option(parcel, src)
    NoticeModel(a)
  }
}

case class ContentAction(
  source: Option[Source],
  sink: Option[Sink]
) extends SourceSinkAction {
  def apply(parcel: Parcel): Parcel = execute_source_sink(parcel) { src =>
    val a = fetch_xml(parcel, src) getOrElse Xml.empty
    XmlModel(a)
  }
}

case class LoginAction(
) extends Action {
  def apply(parcel: Parcel): Parcel = parcel.withContent(RedirectContent())
}

case class LogoutAction(
) extends Action {
  def apply(parcel: Parcel): Parcel = parcel.withContent(RedirectContent())
}

case class BrokenAction(
  message: I18NString,
  json: Option[JsValue],
  jsonError: Option[JsError]
) extends Action {
  def apply(parcel: Parcel): Parcel = parcel
}
object BrokenAction {
  def apply(msg: String, json: JsValue): BrokenAction = BrokenAction(I18NString(msg), Some(json), None)
  def apply(p: JsError): BrokenAction = BrokenAction(JsonUtils.messageI18N(p), None, Some(p))
}

sealed trait Source
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
case class UrlSource(url: URL) extends Source
case class UriSource(uri: URI) extends Source
case class UrnSource(urn: Urn) extends Source {
  def nid = urn.nid
  def module = urn.module
  def submodule = urn.submodule
}
object UrnSource {
  def apply(p: String): UrnSource = UrnSource(Urn(p))
}
case class BrokenSource(v: String) extends Source

sealed trait Sink
object Sink {
  def apply(p: String): Sink = ModelHangerSink(p)
}
case class ModelHangerSink(key: String) extends Sink
