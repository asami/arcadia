package arcadia.context

import org.goldenport.record.v2.Record
import arcadia._
import arcadia.model._
import arcadia.view._
import arcadia.domain._

/*
 * @since   Oct.  8, 2017
 * @version Nov.  1, 2017
 * @author  ASAMI, Tomoharu
 */
case class Query(
  entityType: DomainEntityType,
  start: Int = 0,
  limit: Int = 20,
  maxlimit: Int = 40,
  tags: List[String] = Nil,
  parameters: Map[String, Any] = Map.empty
) {
  def withParameter(params: Option[Record]): Query = params.fold(this)(withParameter)

  def withParameter(params: Record): Query =
    if (params.isEmpty)
      this
    else {
      val et = entityType
      val s = params.getInt('start) getOrElse start
      val l = params.getInt('limit) getOrElse limit
      val ml = params.getInt('maxlimit) getOrElse maxlimit
      val ts = params.getEagerStringList('tags) getOrElse tags
      Query(et, s, l, ml, ts, parameters ++ params.toMap)
    }
}
