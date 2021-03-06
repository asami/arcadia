package arcadia.context

import org.goldenport.record.v3.{IRecord, Record}
import arcadia._
import arcadia.model._
import arcadia.view._
import arcadia.domain._

/*
 * @since   Oct.  8, 2017
 *  version Nov. 23, 2017
 * @version Aug. 31, 2018
 * @author  ASAMI, Tomoharu
 */
case class Query(
  entityType: DomainEntityType,
  start: Int = 0,
  limit: Int = 20,
  maxlimit: Int = 40,
  parameters: IRecord = Record.empty
) {
  def withParameter(params: Option[IRecord]): Query = params.fold(this)(withParameter)

  def withParameter(params: IRecord): Query =
    if (params.isEmpty)
      this
    else {
      val et = entityType
      val s = params.getInt('start) getOrElse start
      val l = params.getInt('limit) getOrElse limit
      val ml = params.getInt('maxlimit) getOrElse maxlimit
//      val ts = params.getEagerStringList('tags) getOrElse tags
      Query(et, s, l, ml, parameters + params)
    }
}

object Query {
  def create(entity: String, params: Option[Map[String, Any]]): Query = {
    val a = Query(DomainEntityType(entity))
    params.map(x => a.withParameter(Record.create(x))).getOrElse(a)
  }
}
