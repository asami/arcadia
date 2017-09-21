package arcadia.context

import java.net.URI
import org.goldenport.record.v2.Record
import arcadia.MimeType
import arcadia.model._
import arcadia.domain._

/*
 * @since   Aug. 29, 2017
 * @version Sep. 21, 2017
 * @author  ASAMI, Tomoharu
 */
trait ExecutionContext {
  def operationName: Option[String]
  def getMimetypeBySuffix(p: Option[String]): Option[MimeType] = p.flatMap(getMimetypeBySuffix)
  def getMimetypeBySuffix(p: String): Option[MimeType]
  def readEntityList(p: Query): EntityListModel
  def createEntity(klass: DomainEntityType, data: Record): DomainObjectId
  def updateEntity(klass: DomainEntityType, id: DomainObjectId, data: Record): Unit
  def deleteEntity(klass: DomainEntityType, id: DomainObjectId): Unit
  def assets: String
  def controllerUri: URI
  def inputFormParameters: Record
  def getFormParameter(key: String): Option[String]
}

case class Query(
  entityType: DomainEntityType,
  start: Int,
  limit: Int,
  maxlimit: Int
)
