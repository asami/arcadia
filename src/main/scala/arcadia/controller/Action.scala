package arcadia.controller

import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.json.JsonUtils
import arcadia._
import arcadia.context._
import arcadia.model._
import arcadia.domain._
import arcadia.scenario.ScenarioEngine

/*
 * @since   Jul. 16, 2017
 *  version Aug. 29, 2017
 *  version Sep. 21, 2017
 * @version Oct. 14, 2017
 * @author  ASAMI, Tomoharu
 */
trait Action {
  def apply(parcel: Parcel): Parcel
}
object Action {
  import org.goldenport.json.JsonUtils.Implicits._
  implicit val OperationActionReads = Json.reads[OperationAction]
  implicit val GetEntityActionReads = Json.reads[GetEntityAction]
  implicit val ReadEntityListActionReads = Json.reads[ReadEntityListAction]

  implicit object ActionReads extends Reads[Action] {
    def reads(json: JsValue): JsResult[Action] = parseJsValue(json)
  }

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
  model: Option[String]
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
  id: String
) extends Action {
  def apply(parcel: Parcel): Parcel = parcel.applyOnContext { context =>
    val r = context.getEntity(DomainEntityType(entityType), StringDomainObjectId(id))
    r.fold(parcel)(parcel.withModel)
  }
}

case class ReadEntityListAction(
  entity: String,
  query: Option[Map[String, Any]],
  form: Option[Map[String, Any]]
) extends Action {
  def apply(parcel: Parcel): Parcel = parcel.applyOnContext { context =>
    val q = Query(DomainEntityType(entity), parameters = query.getOrElse(Map.empty))
    val r = context.readEntityList(q)
    parcel.withModel(r)
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
