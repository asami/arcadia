package arcadia.context

import arcadia._
import arcadia.model._
import arcadia.view._
import arcadia.domain._

/*
 * @since   Oct.  8, 2017
 * @version Oct.  8, 2017
 * @author  ASAMI, Tomoharu
 */
case class Query(
  entityType: DomainEntityType,
  start: Int = 0,
  limit: Int = 20,
  maxlimit: Int = 40,
  tags: List[String] = Nil,
  parameters: Map[String, Any] = Map.empty
)
