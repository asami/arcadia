package arcadia.domain

import java.net.URI
import org.goldenport.record.v2.Record
import org.goldenport.values.ResourceName

/*
 * @since   Aug. 30, 2017
 *  version Sep. 20, 2017
 * @version Oct. 24, 2017
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
  def id: DomainObjectId = StringDomainObjectId(rec.asString(KEY_DOMAIN_OBJECT_ID))
  def name: String = rec.getString(KEY_DOMAIN_OBJECT_NAME) getOrElse ""
  def title: Option[String] = rec.getString(KEY_DOMAIN_OBJECT_TITLE)
  def content: Option[String] = rec.getString(KEY_DOMAIN_OBJECT_CONTENT)
  def imageIcon: Option[URI] = rec.getUri(KEY_DOMAIN_OBJECT_IMAGE_ICON)
  def imagePrimary: Option[URI] = rec.getUri(KEY_DOMAIN_OBJECT_IMAGE_PRIMARY)
}

trait DomainObjectId {
  def v: String
}

case class StringDomainObjectId(v: String) extends AnyRef with DomainObjectId {
  override def toString() = v
}

object DomainObjectId {
  object undefined extends DomainObjectId {
    def v = "id undefined*"
  }
}

case class DomainEntityType(v: String) {
  lazy val resouceName: ResourceName = ResourceName(v)
}
object DomainEntityType {
}

case class DomainEntityId(
  entityType: DomainEntityType,
  id: DomainObjectId,
  getPresentationId: Option[String]
) {
  def presentationId: String = getPresentationId getOrElse id.v
}
