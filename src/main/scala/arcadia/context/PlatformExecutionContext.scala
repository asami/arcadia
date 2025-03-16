package arcadia.context

import scala.xml._
import java.nio.charset.Charset
import java.util.Locale
import java.net.URI
import java.net.URL
import org.joda.time._
import play.api.libs.json.JsValue
import org.goldenport.Platform
import org.goldenport.context._
import org.goldenport.exception.RAISE
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Schema, Column}
import org.goldenport.record.v2.{Invalid, Conclusion}
import org.goldenport.values.PathName
import org.goldenport.io.IoUtils
import org.goldenport.util.DateTimeFormatter
import org.goldenport.util.DateFormatter
import org.goldenport.util.TimeFormatter
import arcadia._
import arcadia.model._
import arcadia.view._
import arcadia.controller.UrnSource
import arcadia.domain._
import arcadia.rule._

/*
 * @since   Aug. 29, 2017
 *  version Sep. 27, 2017
 *  version Oct. 30, 2017
 *  version Nov. 13, 2017
 *  version Jan. 14, 2018
 *  version Mar. 18, 2018
 *  version Jul. 17, 2018
 *  version Aug. 31, 2018
 *  version Apr. 29, 2019
 *  version Mar. 23, 2020
 *  version Apr. 17, 2020
 *  version May. 29, 2020
 *  version Feb. 28, 2022
 *  version May.  2, 2022
 *  version Nov. 27, 2022
 * @version Mar. 12, 2025
 * @author  ASAMI, Tomoharu
 */
trait PlatformExecutionContext {
  def platformContext: PlatformContext
  def locale: Locale
  def dateTimeContext: DateTimeContext
  def formatContext: FormatContext
  // def timezone: DateTimeZone
  // def dateTime: DateTime
  def isLogined: Boolean
  def getOperationName: Option[String]
  def getPathName: Option[PathName]
  def getLogicalUri: Option[URI]
  def getImplicitIndexBase: Option[String]
  def getMimetypeBySuffix(p: Option[String]): Option[MimeType] = p.flatMap(getMimetypeBySuffix)
  def getMimetypeBySuffix(p: String): Option[MimeType] = MimeType.suffixMimeMap.get(p)
  def charsetInputFile: Charset // = Platform.charset.UTF8
  def charsetOutputFile: Charset // = Platform.charset.UTF8
  def charsetConsole: Charset // = Platform.charset.UTF8
  def loadString(p: URL): String = IoUtils.toText(p, charsetInputFile)
  def saveString(p: URL, s: String): Unit = IoUtils.save(p, s, charsetOutputFile)
  def get(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def post(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def post(uri: String, query: IRecord, form: IRecord): Response = post(uri, query.toMap, form.toMap)
  def put(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def delete(uri: String, query: Map[String, Any], form: Map[String, Any]): Response
  def invoke(op: InvokePlatformCommand): Response
  def invoke(op: InvokeOperationCommand): Response
  def execute(op: ExecuteScriptCommand): Response
  def getEntitySchema(name: String): Option[Schema]
  def getDefaultPropertyColumn(name: String): Option[Column]
  def getEntity(entitytype: DomainEntityType, id: DomainObjectId): Option[EntityDetailModel]
  def readEntityList(p: Query): EntityListModel
  def createEntity(klass: DomainEntityType, data: IRecord): DomainObjectId
  def updateEntity(klass: DomainEntityType, id: DomainObjectId, data: IRecord): Unit 
  def deleteEntity(klass: DomainEntityType, id: DomainObjectId): Unit
  def login(username: String, password: String): Either[Conclusion, Session]
  def resetPassword(token: String, password: String, confirmpassword: Option[String]): Either[Conclusion, Unit] = RAISE.unsupportedOperationFault
  def fetchString(urn: UrnSource): Option[String]
  def fetchBadge(urn: UrnSource): Option[Badge]
  def fetchCandidates(name: String): Option[CandidatesModel] = name match {
    case "appid" => Some(CandidatesModel(
      PowertypeClassCandidates(
        PowertypeClassInstance(
          Vector(
            PowertypeInstance("firstAppid"),
            PowertypeInstance("sedondAppid"),
            PowertypeInstance("thirdAppid")
          )
        )
      )
    ))
    case _ => None
  }
  def controllerUri: URI
  def getIdInRequest: Option[DomainObjectId]
  def inputQueryParameters: IRecord
  def inputFormParameters: IRecord
  def getFormParameter(key: String): Option[String]
  def assets: String
  def getResetPasswordRule: Option[ResetPasswordRule] = None

  def formatDateTime(locale: Locale, tz: DateTimeZone, p: DateTime): String = {
    // TODO customizable
    val dtf = DateTimeFormatter.create(locale, tz)
    dtf.format(p)
  }

  def formatDate(locale: Locale, tz: DateTimeZone, p: DateTime): String = {
    // TODO customizable
    val dtf = DateFormatter.create(locale, tz)
    dtf.format(p)
  }

  def formatTime(locale: Locale, tz: DateTimeZone, p: DateTime): String = {
    // TODO customizable
    val dtf = TimeFormatter.create(locale, tz)
    dtf.format(p)
  }
}

object PlatformExecutionContext {
  val develop: PlatformExecutionContext = StandalonePlatformExecutionContext.develop

  class StandalonePlatformExecutionContext(
    val platformContext: PlatformContext,
    val locale: Locale,
    val dateTimeContext: DateTimeContext,
    val formatContext: FormatContext
  ) extends PlatformExecutionContext {
    def isLogined: Boolean = false
    def getOperationName: Option[String] = None
    def getPathName: Option[PathName] = None
    def getLogicalUri: Option[URI] = None
    def getImplicitIndexBase: Option[String] = None
    def charsetInputFile: Charset = Platform.charset.UTF8
    def charsetOutputFile: Charset = Platform.charset.UTF8
    def charsetConsole: Charset = Platform.charset.UTF8
    def get(uri: String, query: Map[String, Any], form: Map[String, Any]): Response = RAISE.notImplementedYetDefect
    def post(uri: String, query: Map[String, Any], form: Map[String, Any]): Response = RAISE.notImplementedYetDefect
    def put(uri: String, query: Map[String, Any], form: Map[String, Any]): Response = RAISE.notImplementedYetDefect
    def delete(uri: String, query: Map[String, Any], form: Map[String, Any]): Response = RAISE.notImplementedYetDefect
    def invoke(op: InvokePlatformCommand): Response = RAISE.notImplementedYetDefect
    def invoke(op: InvokeOperationCommand): Response = RAISE.notImplementedYetDefect
    def execute(op: ExecuteScriptCommand): Response = RAISE.notImplementedYetDefect
    def getEntitySchema(name: String): Option[Schema] = None
    def getDefaultPropertyColumn(name: String): Option[Column] = None
    def getEntity(entitytype: DomainEntityType, id: DomainObjectId): Option[EntityDetailModel] = None
    def readEntityList(p: Query): EntityListModel = RAISE.notImplementedYetDefect
    def createEntity(klass: DomainEntityType, data: IRecord): DomainObjectId = RAISE.notImplementedYetDefect
    def updateEntity(klass: DomainEntityType, id: DomainObjectId, data: IRecord): Unit = RAISE.notImplementedYetDefect
    def deleteEntity(klass: DomainEntityType, id: DomainObjectId): Unit = RAISE.notImplementedYetDefect
    def login(username: String, password: String): Either[Conclusion, Session] = RAISE.notImplementedYetDefect
    def fetchString(urn: UrnSource): Option[String] = None
    def fetchBadge(urn: UrnSource): Option[Badge] = None
    def controllerUri: URI = RAISE.notImplementedYetDefect
    def getIdInRequest: Option[DomainObjectId] = None
    def inputQueryParameters: IRecord = Record.empty
    def inputFormParameters: IRecord = Record.empty
    def getFormParameter(key: String): Option[String] = None
    def assets: String = RAISE.notImplementedYetDefect
  }
  object StandalonePlatformExecutionContext {
    val develop = new StandalonePlatformExecutionContext(
      PlatformContext.develop,
      Locale.getDefault,
      DateTimeContext.now,
      FormatContext.default
    )
  }
}
