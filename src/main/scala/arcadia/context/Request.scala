package arcadia.context

import org.goldenport.values.PathName
import org.goldenport.record.v2.Record
import arcadia.domain.DomainObjectId

/*
 * @since   Dec. 19, 2017
 *  version Dec. 21, 2017
 * @version Mar. 13, 2018
 * @author  ASAMI, Tomoharu
 */
case class Request(
  pathname: String,
  operationName: String,
  method: String, // TODO
  query: Record,
  form: Record
) {
  def isGet = method.toUpperCase == "GET"
  def isMutation = !isGet
  lazy val pathName = PathName(pathname)
  def getDomainObjectId: Option[DomainObjectId] =
    if (pathName.length > 1)
      Some(DomainObjectId(pathName.lastConcreteComponent))
    else
      None
}
