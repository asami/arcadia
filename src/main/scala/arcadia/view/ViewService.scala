package arcadia.view

import play.api.libs.json.JsValue
import org.goldenport.record.v3.Record
import arcadia._
import arcadia.context.{Query => CQuery, _}
import arcadia.domain._
import arcadia.model._
import arcadia.view.value._

/*
 * @since   Sep. 25, 2017
 *  version Oct. 10, 2017
 *  version Nov.  3, 2017
 *  version Sep.  1, 2018
 *  version May.  4, 2022
 * @version Sep. 30, 2023
 * @author  ASAMI, Tomoharu
 */
case class ViewService(
  context: ExecutionContext,
  strategy: RenderStrategy,
  properties: Option[PropertySheetModel]
) {
  def get(uri: String, query: Map[String, Any] = Map.empty, form: Map[String, Any] = Map.empty): ViewResponse = ViewResponse(context.get(uri, query, form), strategy)
  def getJson(uri: String, query: Map[String, Any] = Map.empty, form: Map[String, Any] = Map.empty): JsValue = get(uri, query, form).json
  def post(uri: String, query: Map[String, Any] = Map.empty, form: Map[String, Any] = Map.empty): ViewResponse = ViewResponse(context.post(uri, query, form), strategy)
  def postJson(uri: String, query: Map[String, Any] = Map.empty, form: Map[String, Any] = Map.empty): JsValue = post(uri, query, form).json
  def put(uri: String, query: Map[String, Any] = Map.empty, form: Map[String, Any] = Map.empty): ViewResponse = ViewResponse(context.put(uri, query, form), strategy)
  def putJson(uri: String, query: Map[String, Any] = Map.empty, form: Map[String, Any] = Map.empty): JsValue = put(uri, query, form).json
  def delete(uri: String, query: Map[String, Any] = Map.empty, form: Map[String, Any] = Map.empty): ViewResponse = ViewResponse(context.delete(uri, query, form), strategy)
  def deleteJson(uri: String, query: Map[String, Any] = Map.empty, form: Map[String, Any] = Map.empty): JsValue = delete(uri, query, form).json

  def getEntity(entitytype: String, id: String): Option[ViewEntity] = strategy.execute { ctx =>
    ctx.getEntity(DomainEntityType(entitytype), StringDomainObjectId(id)).map(ViewEntity(_, strategy))
  }

  def readEntityList(
    name: String,
    start: Int = 0,
    limit: Int = 20,
    parameters: Map[String, Any] = Map.empty
  ): ViewEntityList = strategy.execute { ctx =>
    val q = CQuery(
      DomainEntityType(name),
      start, limit,
      parameters = Record.create(parameters)
    )
    ViewEntityList(ctx.readEntityList(q), strategy)
  }

  def readEntityList(query: Query): ViewEntityList = strategy.execute { ctx =>
    val q = query.toQueryForExecutionContext
    ViewEntityList(ctx.readEntityList(q), strategy)
  }

  def dateTime: ViewDateTime = {
    val dt = context.dateTime
    val s = strategy.formatDateTime(dt)
    ViewDateTime(dt, s)
  }

  def date: ViewDate = {
    val dt = context.dateTime
    val s = strategy.formatDate(dt)
    ViewDate(dt, s)
  }

  def time: ViewTime = {
    val dt = context.dateTime
    val s = strategy.formatTime(dt)
    ViewTime(dt, s)
  }

  def get(key: String): ViewValue = _get_option(key).getOrElse(ViewEmpty)

  def get(key: Option[String]): ViewValue = key.flatMap(_get_option).getOrElse(ViewEmpty)

  def _get_option(key: String): Option[ViewValue] = properties.flatMap(_.record.get(key)).map(_view)

  private def _view(p: Any): ViewValue = p match {
    case m: ViewValue => m
    case m: String => ViewString(m)
    case m: Number => ViewNumber(m)
    case m => ViewBean(m)
  }
}

case class Query(
  name: String,
  start: Int = 0,
  limit: Int = 20,
  maxlimit: Int = 20,
  parameters: Map[String, Any] = Map.empty
) {
  def toQueryForExecutionContext: CQuery = CQuery(
    DomainEntityType(name),
    start, limit, maxlimit,
    None,
    Record.create(parameters)
  )
}
