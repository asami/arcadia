package arcadia.context

import org.goldenport.record.v3.{IRecord, Record}
import arcadia._
import arcadia.model._
import arcadia.view._
import arcadia.domain._

/*
 * @since   Oct.  8, 2017
 *  version Nov. 23, 2017
 *  version Aug. 31, 2018
 *  version Mar. 30, 2023
 *  version Sep. 30, 2023
 * @version Oct. 31, 2023
 * @author  ASAMI, Tomoharu
 */
case class Query(
  entityType: DomainEntityType,
  offset: Int = 0,
  limit: Int = 20,
  maxlimit: Int = 40,
  columns: Option[List[String]] = None,
  paging: Option[Query.Paging] = None,
  parameters: IRecord = Record.empty
) {
  def withParameter(params: Option[IRecord]): Query = params.fold(this)(withParameter)

  def withParameter(params: IRecord): Query =
    if (params.isEmpty)
      this
    else {
      val et = entityType
      val s = params.getInt("query.offset") getOrElse offset
      val l = params.getInt("query.limit") getOrElse limit
      val ml = params.getInt("query.maxlimit") getOrElse maxlimit
      val cs = params.getEagerTokenList("query.columns")
      //      val ts = params.getEagerStringList('tags) getOrElse tags
      val paging = params.getInt("query.page.size").map(Query.Paging)
      Query(et, s, l, ml, cs, paging, parameters + params)
    }
}

object Query {
  case class Paging(size: Int)

  def create(entity: String, params: Option[Map[String, Any]]): Query =
    create(DomainEntityType(entity), params)

  def create(entity: DomainEntityType, params: Option[Map[String, Any]]): Query = {
    val a = Query(entity)
    params.map(x => a.withParameter(Record.create(x))).getOrElse(a)
  }
}
