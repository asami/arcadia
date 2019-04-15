package arcadia.domain

import java.net.URI
import org.goldenport.exception.RAISE
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => Record2}
import org.goldenport.values.ResourceName
import arcadia.model.Picture

/*
 * @since   Aug. 30, 2017
 *  version Sep. 20, 2017
 *  version Oct. 24, 2017
 *  version Nov.  5, 2017
 *  version Dec. 17, 2017
 *  version Jan. 14, 2018
 *  version Mar. 13, 2018
 *  version Aug. 31, 2018
 *  version Sep.  1, 2018
 * @version Nov.  7, 2018
 * @author  ASAMI, Tomoharu
 */
trait DomainObject {
  def id: DomainObjectId
  def name: String
  def title: Option[String]
  def content: Option[String]
  def imageIcon: Option[Picture]
  def imagePrimary: Option[Picture]
  def record: IRecord
}

case class RecordDomainObject(record: IRecord) extends DomainObject {
  def id: DomainObjectId = DomainObjectId.get(record).getOrElse {
    RAISE.notImplementedYetDefect
  }
  def name: String = record.getString(KEY_DOMAIN_OBJECT_NAME) getOrElse ""
  def title: Option[String] = record.getString(KEY_DOMAIN_OBJECT_TITLE)
  def content: Option[String] = record.getString(KEY_DOMAIN_OBJECT_CONTENT)
  def imageIcon: Option[Picture] = Picture.get(record, KEY_DOMAIN_OBJECT_IMAGE_ICON)
  def imagePrimary: Option[Picture] = Picture.get(record, KEY_DOMAIN_OBJECT_IMAGE_PRIMARY)
}

trait DomainObjectId {
  def v: String
  def getPresentationId: Option[String]
  def getEntityType: Option[DomainEntityType]
  def presentationId = getPresentationId getOrElse v
}
object DomainObjectId {
  val undefined = StringDomainObjectId("*id undefined*")

  def apply(id: String): DomainObjectId = StringDomainObjectId(id)

  def get(
    p: IRecord,
    defaultentity: Option[DomainEntityType] = None
  ): Option[DomainObjectId] = {
    p.getString(KEY_DOMAIN_OBJECT_ID) map { id =>
      val entitytype = p.getString(KEY_DOMAIN_OBJECT_ENTITYTYPE).
        map(DomainEntityType(_)) orElse defaultentity
      entitytype.map { x =>
        def presentationid = p.getString(KEY_DOMAIN_OBJECT_PRESENTATION_ID)
        DomainEntityId(StringDomainObjectId(id), x, presentationid)
      }.getOrElse(StringDomainObjectId(id))
    }
  }
}

case class StringDomainObjectId(v: String) extends AnyRef with DomainObjectId {
  def getPresentationId: Option[String] = None
  def getEntityType: Option[DomainEntityType] = None
  override def toString() = v
}

case class DomainEntityType(v: String) {
  lazy val resouceName: ResourceName = ResourceName(v)
}
object DomainEntityType {
}

case class DomainEntityId(
  id: DomainObjectId,
  entityType: DomainEntityType,
  getPresentationId: Option[String] = None
) extends DomainObjectId {
  def getEntityType = Some(entityType)
  def v = id.v
}
object DomainEntityId {
  def get(
    p: IRecord,
    defaultentity: Option[DomainEntityType] = None
  ): Option[DomainEntityId] = DomainObjectId.get(p, defaultentity) flatMap {
    case m: DomainEntityId => Some(m)
    case _ => None
  }
}
