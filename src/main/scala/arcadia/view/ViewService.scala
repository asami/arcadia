package arcadia.view

import play.api.libs.json.JsValue
import arcadia._
import arcadia.context.{Query => CQuery, _}
import arcadia.domain._
import arcadia.model._

/*
 * @since   Sep. 25, 2017
 * @version Sep. 27, 2017
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

  def readEntityList(
    name: String,
    start: Int = 0,
    limit: Int = 20,
    tags: List[String] = Nil,
    parameters: Map[String, Any] = Map.empty
  ): ViewEntityList = strategy.execute { ctx =>
    val q = CQuery(
      DomainEntityType(name),
      start, limit,
      tags = tags,
      parameters = parameters
    )
    ViewEntityList(ctx.readEntityList(q), strategy)
  }

  def readEntityList(query: Query): ViewEntityList = strategy.execute { ctx =>
    val q = CQuery(
      DomainEntityType(query.name),
      query.start, query.limit,
      tags = query.tags,
      parameters = query.parameters
    )
    ViewEntityList(ctx.readEntityList(q), strategy)
  }
}


case class Query(
  name: String,
  start: Int = 0,
  limit: Int = 20,
  maxlimit: Int = 20,
  tags: List[String] = Nil,
  parameters: Map[String, Any] = Map.empty
) {
  def toQueryForExecutionContext: CQuery = CQuery(
    DomainEntityType(name),
    start, limit, maxlimit,
    tags,
    parameters
  )
}
