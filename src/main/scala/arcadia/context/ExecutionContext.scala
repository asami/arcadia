package arcadia.context

import scala.xml._
import java.util.TimeZone
import java.net.URI
import java.net.URL
import org.joda.time._
import play.api.libs.json.JsValue
import org.goldenport.exception.RAISE
import org.goldenport.extension.Showable
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Schema, Column}
import org.goldenport.record.v2.{Invalid, Conclusion}
import org.goldenport.record.v2.util.SchemaBuilder
import org.goldenport.values.PathName
import arcadia._
import arcadia.model._
import arcadia.view._
import arcadia.controller.UrnSource
import arcadia.domain._
import arcadia.rule._
import arcadia.service.ServiceFacility

/*
 * @since   Aug. 29, 2017
 *  version Sep. 27, 2017
 *  version Oct. 30, 2017
 *  version Nov. 13, 2017
 *  version Jan. 15, 2018
 *  version Jul. 17, 2018
 *  version Aug. 31, 2018
 *  version Sep.  5, 2018
 *  version Apr. 29, 2019
 *  version Mar. 23, 2020
 *  version Apr. 17, 2020
 *  version May. 29, 2020
 *  version Feb. 27, 2022
 *  version Mar. 21, 2022
 *  version Apr. 30, 2022
 *  version May.  2, 2022
 *  version Oct. 30, 2022
 * @version Nov. 27, 2022
 * @author  ASAMI, Tomoharu
 */
case class ExecutionContext(
  platformExecutionContext: PlatformExecutionContext,
  services: ServiceFacility,
  webapp: WebApplication
) {
  def config = webapp.config

  // TODO request locale
  lazy val locale = webapp.getLocale getOrElse platformExecutionContext.locale

  def dateTimeContext = platformExecutionContext.dateTimeContext
  def formatContext = webapp.getFormatContext getOrElse platformExecutionContext.formatContext

  def isLogined: Boolean = platformExecutionContext.isLogined
  def getOperationName: Option[String] = platformExecutionContext.getOperationName
  def getPathName: Option[PathName] = platformExecutionContext.getPathName
  def getLogicalUri: Option[URI] = platformExecutionContext.getLogicalUri
  def resolvePathName(pn: PathName): PathName = getPathName.fold(pn)(reqpath =>
    if (reqpath.components.length >= 2)
      List.fill(reqpath.components.length - 1)("..").mkString("/") +: pn
    else
      pn
  )
  def getMimetypeBySuffix(p: Option[String]): Option[MimeType] = platformExecutionContext.getMimetypeBySuffix(p)
  def getMimetypeBySuffix(p: String): Option[MimeType] = platformExecutionContext.getMimetypeBySuffix(p)
  def loadString(p: URL): String = platformExecutionContext.loadString(p)

  def get(uri: String, query: Option[Map[String, Any]], form: Option[Map[String, Any]]): Response = platformExecutionContext.get(uri, query.getOrElse(Map.empty), form.getOrElse(Map.empty))
  def get(uri: String, query: Map[String, Any], form: Map[String, Any]): Response = platformExecutionContext.get(uri, query, form)
  def post(uri: String, query: Option[Map[String, Any]], form: Option[Map[String, Any]]): Response = platformExecutionContext.post(uri, query.getOrElse(Map.empty), form.getOrElse(Map.empty))
  def post(uri: String, query: Map[String, Any], form: Map[String, Any]): Response = platformExecutionContext.post(uri, query, form)
  def post(uri: String, form: IRecord): Response = platformExecutionContext.post(uri, Record.empty, form)
  def post(uri: String, query: IRecord, form: IRecord): Response = platformExecutionContext.post(uri, query, form)
  def put(uri: String, query: Option[Map[String, Any]], form: Option[Map[String, Any]]): Response = platformExecutionContext.put(uri, query.getOrElse(Map.empty), form.getOrElse(Map.empty))
  def put(uri: String, query: Map[String, Any], form: Map[String, Any]): Response = platformExecutionContext.put(uri, query, form)
  def delete(uri: String, query: Option[Map[String, Any]], form: Option[Map[String, Any]]): Response = platformExecutionContext.delete(uri, query.getOrElse(Map.empty), form.getOrElse(Map.empty))
  def delete(uri: String, query: Map[String, Any], form: Map[String, Any]): Response = platformExecutionContext.delete(uri, query, form)

  def invoke(op: InvokePlatformCommand): Response = platformExecutionContext.invoke(op)

  def invoke(op: InvokeOperationCommand): Response =
    services.invokeOption(op) getOrElse platformExecutionContext.invoke(op)

  def execute(cmd: ExecuteScriptCommand): Response = platformExecutionContext.execute(cmd)

  def getEntitySchema(name: String): Option[Schema] = {
    // TODO config
    platformExecutionContext.getEntitySchema(name)
  }
  def getDefaultPropertyColumn(name: String): Option[Column] = {
    // TODO config
    platformExecutionContext.getDefaultPropertyColumn(name)
  }
  def getEntity(entitytype: DomainEntityType, id: DomainObjectId): Option[EntityDetailModel] = platformExecutionContext.getEntity(entitytype, id)
  def readEntityList(p: Query): EntityListModel = platformExecutionContext.readEntityList(p)
  def createEntity(klass: DomainEntityType, data: IRecord): DomainObjectId = platformExecutionContext.createEntity(klass, data)
  def updateEntity(klass: DomainEntityType, id: DomainObjectId, data: IRecord): Unit = platformExecutionContext.updateEntity(klass, id, data)
  def deleteEntity(klass: DomainEntityType, id: DomainObjectId): Unit = platformExecutionContext.deleteEntity(klass, id)

  def login(username: String, password: String): Either[Conclusion, Session] = platformExecutionContext.login(username, password)
  def resetPassword(token: String, password: String, confirmpassword: Option[String]): Either[Conclusion, Unit] = platformExecutionContext.resetPassword(token, password, confirmpassword)

  def fetchString(urn: UrnSource): Option[String] = platformExecutionContext.fetchString(urn)
  def fetchBadge(urn: UrnSource): Option[Badge] = platformExecutionContext.fetchBadge(urn)
  def fetchCandidates(name: String): Option[CandidatesModel] = platformExecutionContext.fetchCandidates(name)

  def controllerUri: URI = platformExecutionContext.controllerUri
  def getIdInRequest: Option[DomainObjectId] = platformExecutionContext.getIdInRequest
  lazy val inputQueryParameters: IRecord = platformExecutionContext.inputQueryParameters.toRecord.http.request.normalizePlain
  lazy val inputFormParameters: IRecord = platformExecutionContext.inputFormParameters.toRecord.http.request.normalizePlain
  def getFormParameter(key: String): Option[String] = inputFormParameters.getString(key)
  lazy val assets: String = config.getAssets getOrElse platformExecutionContext.assets

  def dateTime: DateTime = dateTimeContext.current
  def timezone: TimeZone = dateTimeContext.timezone
  def dateTimeZone: DateTimeZone = dateTimeContext.dateTimeZone

  // def format(p: Any): String = p match {
  //   case m: Showable => m.print
  //   case m: DateTime => webapp.format(locale, dateTimeZone, m) getOrElse platformExecutionContext.formatDateTime(locale, dateTimeZone, m)
  //   case m => m.toString
  // }

  // def formatDateTime(p: DateTime): String =
  //   webapp.formatDateTime(locale, dateTimeZone, p) getOrElse platformExecutionContext.formatDateTime(locale, dateTimeZone, p)

  // def formatDate(p: DateTime): String =
  //   webapp.formatDate(locale, dateTimeZone, p) getOrElse platformExecutionContext.formatDate(locale, dateTimeZone, p)

  // def formatTime(p: DateTime): String =
  //   webapp.formatTime(locale, dateTimeZone, p) getOrElse platformExecutionContext.formatTime(locale, dateTimeZone, p)

  def toCode(e: Throwable): Int = ExecutionContext.toCode(e)

  def resetPasswordRule: ResetPasswordRule =
    platformExecutionContext.getResetPasswordRule getOrElse ResetPasswordRule.default
}

object ExecutionContext {
  def toCode(e: Throwable): Int = {
    e match {
      case m: IllegalArgumentException => 400 // BadRequest
      case m: NoSuchElementException => 404 // NotFound
      case m: UnsupportedOperationException => 501 // NotImplemented
      case m => 500 // InternalServerError
    }
  }
}
