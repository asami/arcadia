package arcadia.context

import org.goldenport.values.ResourceName
import arcadia.MimeType
import arcadia.model._

/*
 * @since   Aug. 29, 2017
 * @version Sep.  4, 2017
 * @author  ASAMI, Tomoharu
 */
trait ExecutionContext {
  def operationName: Option[String]
  def getMimetypeBySuffix(p: Option[String]): Option[MimeType] = p.flatMap(getMimetypeBySuffix)
  def getMimetypeBySuffix(p: String): Option[MimeType]
  def readResourceList(p: Query): ResourceListModel
}

case class Query(
  resource: ResourceName,
  start: Int,
  limit: Int,
  maxlimit: Int
)
