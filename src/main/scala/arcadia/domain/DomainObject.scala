package arcadia.domain

import java.net.URI
import org.goldenport.record.v2.Record
import org.goldenport.values.ResourceName

/*
 * @since   Aug. 30, 2017
 * @version Sep. 20, 2017
 * @author  ASAMI, Tomoharu
 */
trait DomainObject {
  def id: DomainObjectId
  def name: String
  def title: Option[String]
  def content: Option[String]
  def imageIcon: Option[URI]
  def imagePrimary: Option[URI]
}

case class RecordDomainObject(rec: Record) extends DomainObject {
  def id: DomainObjectId = StringDomainObjectId(rec.asString(KEY_OBJECT_ID))
  def name: String = rec.getString(KEY_OBJECT_NAME) getOrElse ""
  def title: Option[String] = rec.getString(KEY_OBJECT_TITLE)
  def content: Option[String] = rec.getString(KEY_OBJECT_CONTENT)
  def imageIcon: Option[URI] = rec.getUri(KEY_OBJECT_IMAGE_ICON)
  def imagePrimary: Option[URI] = rec.getUri(KEY_OBJECT_IMAGE_PRIMARY)
}

trait DomainObjectId {
  def s: String
}

case class StringDomainObjectId(s: String) extends AnyRef with DomainObjectId {
  override def toString() = s
}

object DomainObjectId {
  object undefined extends DomainObjectId {
    def s = "id undefined*"
  }
}

case class DomainEntityType(v: String) {
  lazy val resouceName: ResourceName = ResourceName(v)
}
object DomainEntityType {
}

case class DomainEntityId(klass: DomainEntityType, id: DomainObjectId)
