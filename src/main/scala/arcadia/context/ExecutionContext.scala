package arcadia.context

import scala.xml._
import java.net.URI
import play.api.libs.json.JsValue
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.Record
import org.goldenport.record.v2.util.SchemaBuilder
import arcadia._
import arcadia.model._
import arcadia.view._
import arcadia.domain._

/*
 * @since   Aug. 29, 2017
 * @version Sep. 27, 2017
 * @author  ASAMI, Tomoharu
 */
trait ExecutionContext {
  def operationName: Option[String]
  def getMimetypeBySuffix(p: Option[String]): Option[MimeType] = p.flatMap(getMimetypeBySuffix)
  def getMimetypeBySuffix(p: String): Option[MimeType]
  def get(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def post(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def put(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def delete(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
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
  start: Int = 0,
  limit: Int = 20,
  maxlimit: Int = 40,
  tags: List[String] = Nil,
  parameters: Map[String, Any] = Map.empty
)

trait Response {
  def code: Int
  def mime: String
  def entityType: Option[DomainEntityType]
  def getString: Option[String]
  def getRecord: Option[Record]
  def getRecords: Option[List[Record]]
  def transfer: Option[Transfer]
  def json: JsValue

  def render(strategy: RenderStrategy): NodeSeq = {
    import SchemaBuilder._
    PropertySheetModel(
      "Response",
      SchemaBuilder.create(
        CO("code"),
        CO("mime"),
        CO("entity"),
        CO("string"),
        CO("record"),
        CO("records"),
        CO("transfer"),
        CO("json")
      ),
      Record.dataApp(
        "code" -> code,
        "mime" -> mime,
        "entity" -> entityType,
        "string" -> getString,
        "record" -> getRecord,
        "records" -> getRecords,
        "transfer" -> transfer,
        "json" -> json
      )
    ).render(strategy)
  }

  def asPropertySheet: PropertySheetModel = getRecord.
    map(PropertySheetModel(None, None, _)).
    getOrElse(RAISE.unsupportedOperationFault) // TODO
  def asPropertyTable: PropertyTableModel = getRecords.
    map(PropertyTableModel(None, None, _)).
    getOrElse(RAISE.unsupportedOperationFault) // TODO
  def asEntityDetail: EntityDetailModel = (entityType, getRecord) match {
    case (Some(entity), Some(rec)) => EntityDetailModel(None, entity, None, rec)
    case (_, _) => RAISE.unsupportedOperationFault // TODO
  }
  def asEntityList: EntityListModel = (entityType, getRecords, transfer) match {
    case (Some(entity), Some(rs), Some(transfer)) => EntityListModel(None, entity, None, rs, transfer)
    case (_, _, _) => RAISE.unsupportedOperationFault // TODO
  }
}
