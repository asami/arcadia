package arcadia.view

import play.api.libs.json.JsValue
import org.goldenport.record.v2.Record
import arcadia._
import arcadia.context.{Query => CQuery, _}
import arcadia.domain._
import arcadia.model._

/*
 * @since   Sep. 25, 2017
 *  version Oct. 10, 2017
 * @version Nov.  3, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewService(context: ExecutionContext, strategy: RenderStrategy) {
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
    Record.create(parameters)
  )
}
