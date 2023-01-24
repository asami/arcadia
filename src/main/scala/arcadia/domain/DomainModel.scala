package arcadia.domain

import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.values.PathName
import arcadia._
import arcadia.context._
import arcadia.model._

/*
 * @since   Dec.  4, 2022
 *  version Dec. 31, 2022
 * @version Jan.  1, 2023
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
}

object DomainModel {
  val empty = new DomainModel() {
  }

  sealed trait Strategy
  object Strategy {
    case class ReadEntityList(entity: String) extends Strategy
    case class GetEntity(entity: String, id: String) extends Strategy
    case object CreateEntity extends Strategy
    case object UpdateEntity extends Strategy
    case object DeleteEntity extends Strategy
    case object Skip extends Strategy
  }

  def apply(p: DomainModelSpace, ps: DomainModelSpace*): DomainModel = new DomainModel(p +: ps)
}
