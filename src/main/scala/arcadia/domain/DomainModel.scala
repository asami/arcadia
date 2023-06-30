package arcadia.domain

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.values.PathName
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.Schema
import arcadia._
import arcadia.context._
import arcadia.model._

/*
 * @since   Dec.  4, 2022
 *  version Dec. 31, 2022
 *  version Jan. 29, 2023
 *  version Mar. 30, 2023
 *  version Apr. 16, 2023
 * @version Jun. 24, 2023
 * @author  ASAMI, abstraclass
 */
class DomainModel(initialspaces: Seq[DomainModelSpace] = Vector.empty) {
  import DomainModel._

  private var _spaces: Vector[DomainModelSpace] = initialspaces.toVector

  def add(rhs: DomainModel): DomainModel = {
    _spaces = _spaces ++ rhs._spaces
    this
  }

  def add(rhs: DomainModelSpace): DomainModel = {
    _spaces = _spaces :+ rhs
    this
  }

  def isAvailableResource(pathname: PathName): Boolean =
    _spaces.exists(_.isAvailableResource(pathname))

  def strategy(parcel: Parcel, pathname: PathName): Strategy =
    _spaces.toStream.flatMap(_.strategy(parcel, pathname)).headOption.
      getOrElse(Strategy.Skip)

  def getEntitySchema(
    entitytype: DomainEntityType
  ): Option[Schema] =
    _spaces.toStream.flatMap(_.getEntitySchema(entitytype)).headOption

  def getEntity(
    entitytype: DomainEntityType,
    id: DomainObjectId
  ): Consequence[Option[EntityDetailModel]] = {
    _spaces.toStream.flatMap(_.getEntity(entitytype, id)).headOption.
      getOrElse(???)
  }

  def readEntityList(q: Query): Consequence[EntityListModel] = {
    _spaces.toStream.flatMap(_.readEntityList(q)).headOption.
      getOrElse(???)
  }

  def createEntity(
    entitytype: DomainEntityType,
    record: IRecord
  ): Consequence[DomainObjectId] = _spaces.toStream.flatMap(_.createEntity(entitytype, record)).headOption.getOrElse(???)

  def updateEntity(
    entitytype: DomainEntityType,
    id: DomainObjectId,
    record: IRecord
  ): Consequence[Unit] = _spaces.toStream.flatMap(_.updateEntity(entitytype, id, record)).headOption.getOrElse(???)

  def deleteEntity(
    entitytype: DomainEntityType,
    id: DomainObjectId
  ): Consequence[Unit] = _spaces.toStream.flatMap(_.deleteEntity(entitytype, id)).headOption.getOrElse(???)
}

object DomainModel {
  implicit lazy val DomainModelMonoid: Monoid[DomainModel] = new Monoid[DomainModel] {
    def append(lhs: DomainModel, rhs: => DomainModel): DomainModel = lhs add rhs
    def zero: DomainModel = empty
  }

  val empty = new DomainModel() {
  }

  sealed trait Strategy
  object Strategy {
    case class ReadEntityList(entity: DomainEntityType) extends Strategy
    case class GetEntity(entity: DomainEntityType, id: DomainObjectId) extends Strategy
    case class CreateEntity(entity: DomainEntityType) extends Strategy
    case class UpdateEntity(entity: DomainEntityType, id: Option[DomainObjectId]) extends Strategy
    case class DeleteEntity(entity: DomainEntityType, id: Option[DomainObjectId]) extends Strategy
    case object Skip extends Strategy
  }

  def apply(p: DomainModelSpace, ps: DomainModelSpace*): DomainModel = new DomainModel(p +: ps)
}
