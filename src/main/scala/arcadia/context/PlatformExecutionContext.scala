package arcadia.context

import scala.xml._
import java.net.URI
import play.api.libs.json.JsValue
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.{Record, Schema, Column}
import arcadia._
import arcadia.model._
import arcadia.view._
import arcadia.controller.UrnSource
import arcadia.domain._

/*
 * @since   Aug. 29, 2017
 *  version Sep. 27, 2017
 *  version Oct. 30, 2017
 *  version Nov. 13, 2017
 * @version Jan.  7, 2018
 * @author  ASAMI, Tomoharu
 */
trait PlatformExecutionContext {
  def isLogined: Boolean
  def getOperationName: Option[String]
  def getMimetypeBySuffix(p: Option[String]): Option[MimeType] = p.flatMap(getMimetypeBySuffix)
  def getMimetypeBySuffix(p: String): Option[MimeType]
  def get(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def post(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def put(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def delete(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def invoke(op: InvokeCommand): Response
  def getEntitySchema(name: String): Option[Schema]
  def getDefaultPropertyColumn(name: String): Option[Column]
  def getEntity(entitytype: DomainEntityType, id: DomainObjectId): Option[EntityDetailModel]
  def readEntityList(p: Query): EntityListModel
  def createEntity(klass: DomainEntityType, data: Record): DomainObjectId
  def updateEntity(klass: DomainEntityType, id: DomainObjectId, data: Record): Unit 
  def deleteEntity(klass: DomainEntityType, id: DomainObjectId): Unit
  def fetchString(urn: UrnSource): Option[String]
  def fetchBadge(urn: UrnSource): Option[Badge]
  def controllerUri: URI
  def getIdInRequest: Option[DomainObjectId]
  def inputQueryParameters: Record
  def inputFormParameters: Record
  def getFormParameter(key: String): Option[String]
  def assets: String
}
