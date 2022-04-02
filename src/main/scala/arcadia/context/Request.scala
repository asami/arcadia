package arcadia.context

import org.goldenport.values.PathName
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => Record2}
import org.goldenport.util.StringUtils
import arcadia.domain.DomainObjectId

/*
 * @since   Dec. 19, 2017
 *  version Dec. 21, 2017
 *  version Mar. 13, 2018
 *  version Mar. 23, 2020
 * @version Mar.  6, 2022
 * @author  ASAMI, Tomoharu
 */
case class Request(
  pathname: String,
  operationName: String,
  method: String, // TODO
  query: IRecord,
  form: IRecord
) {
  def isGet = method.toUpperCase == "GET"
  def isMutation = !isGet
  lazy val pathName = PathName(pathname)
  def getDomainObjectId: Option[DomainObjectId] =
    if (pathName.length > 1)
      Some(DomainObjectId(pathName.leaf))
    else
      None

  def operationUriString: String = StringUtils.concatPath(pathname, operationName)
}

object Request {
  def apply(
    pathname: String,
    operationName: String,
    method: String,
    query: Record2,
    form: Record2
  ): Request = new Request(
    pathname,
    operationName,
    method,
    Record.create(query),
    Record.create(form)
  )
}
