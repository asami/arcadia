package arcadia.view

import scala.language.dynamics
import scala.xml._
import org.goldenport.record.v2._
import org.goldenport.exception.RAISE

/*
 * @since   Jul. 16, 2017
 *  version Aug. 30, 2017
 * @version Dec. 17, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewRecord(record: Record, strategy: RenderStrategy) extends Dynamic {
  def getObject(name: String): Option[Any] = record.getOne(name)
  def get(name: String): Option[String] = record.getOne(name).map(strategy.format)
  def getDateTime(name: String): Option[String] = get(name).map(strategy.formatDateTime)
  def getDate(name: String): Option[String] = get(name).map(strategy.formatDate)
  def getTime(name: String): Option[String] = get(name).map(strategy.formatTime)
  def getXml(name: String): Option[NodeSeq] = get(name).map(strategy.formatXml)
  def as(name: String): String = get(name) getOrElse ""
  def as(name: String, fallback: String): String = get(name) getOrElse fallback
  def asDateTime(name: String): String = getDateTime(name) getOrElse ""
  def asDate(name: String): String = getDate(name) getOrElse ""
  def asTime(name: String): String = getTime(name) getOrElse ""
  def asXml(name: String): NodeSeq = getXml(name) getOrElse Group(Nil)
  def selectDynamic(name: String): Any = record.getOne(name) getOrElse ""
}

object ViewRecord {
  def create(rec: Record, strategy: RenderStrategy): ViewRecord = ViewRecord(rec, strategy)
}
