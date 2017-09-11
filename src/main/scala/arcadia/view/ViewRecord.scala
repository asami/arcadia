package arcadia.view

import scala.language.dynamics
import org.goldenport.record.v2._
import org.goldenport.exception.RAISE

/*
 * @since   Jul. 16, 2017
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
case class ViewRecord(record: Record) extends Dynamic {
  def get(name: String): Option[Any] = record.getOne(name)
  def selectDynamic(name: String): Any = record.getOne(name) getOrElse {
    RAISE.missingPropertyFault(name)
  }
}

object ViewRecord {
  def create(rec: Record): ViewRecord = ViewRecord(rec)
}
