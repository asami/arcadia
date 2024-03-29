package arcadia.context

import scala.xml._
import play.api.libs.json.{JsValue, JsNull}
import org.goldenport.exception.RAISE
import org.goldenport.context.StatusCode
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.XString
import org.goldenport.record.v2.util.SchemaBuilder
import arcadia._
import arcadia.model._
import arcadia.view._
import arcadia.view.tag.Tag
import arcadia.domain._

/*
 * @since   Oct.  8, 2017
 *  version Aug. 31, 2018
 *  version Nov.  7, 2018
 *  version Apr. 30, 2019
 *  version Mar. 20, 2022
 *  version Sep. 26, 2022
 * @version Oct. 30, 2022
 * @author  ASAMI, Tomoharu
 */
trait Response {
  def code: Int
  def mime: String
  def entityType: Option[DomainEntityType]
  def getString: Option[String]
  def getRecord: Option[IRecord]
  def getRecords: Option[List[IRecord]]
  def transfer: Option[Transfer]
  def json: JsValue

  def isSuccess: Boolean = code == 200
  def isNotFound: Boolean = code == StatusCode.NotFound.code

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
      Record.data(
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

  def toModel: Model =
    if (isSuccess) {
      getRecords.
        map(PropertyTableModel(None, None, _)).
        getOrElse {
          getRecord.map(PropertySheetModel(None, None, _)).
            getOrElse {
              getString.map(x => SingleValueModel(XString, Some(x))). // XXX
                getOrElse {
                  EmptyModel // XXX
                }
            }
        }
    } else {
      ErrorModel.create(this)
    }
}

object Response {
  case class Standard(
    code: Int = StatusCode.Ok.code,
    mime: String = MimeType.APPLICATION_JSON.name,
    entityType: Option[DomainEntityType] = None,
    getString: Option[String] = None,
    getRecord: Option[IRecord] = None,
    getRecords: Option[List[IRecord]] = None,
    transfer: Option[Transfer] = None,
    json: JsValue = JsNull
  ) extends Response {
  }

  def record(p: IRecord): Response = Standard(getRecord = Some(p))
  def notFound(): Response = Standard(StatusCode.NotFound.code)
  def error(code: Int, msg: String): Response = Standard(code, getString = Some(msg))
}
