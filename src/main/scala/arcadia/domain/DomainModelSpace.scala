package arcadia.domain

import java.io.File
import org.goldenport.context.Consequence
import org.goldenport.tree.Tree
import org.goldenport.values.PathName
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.Schema
import arcadia._
import arcadia.context._
import arcadia.model._
import arcadia.domain.DomainModel.Strategy

/*
 * @since   Jan.  1, 2023
 *  version Jan.  1, 2023
 *  version Mar. 30, 2023
 * @version Apr. 16, 2023
 * @author  ASAMI, abstraclass
 */
trait DomainModelSpace {
  import DomainModelSpace._

  val classes = Tree.create[DomainClass]()

  def isAvailableResource(pathname: PathName): Boolean =
    classes.getNode(pathname.v) match {
      case Some(s) => s.isLeaf
      case None => pathname.getParent.fold(false)(x => classes.getNode(x.v).isDefined)
    }

  def strategy(parcel: Parcel, pathname: PathName): Option[Strategy] = {
    classes.getNode(pathname.v) match {
      case Some(s) =>
        if (s.isLeaf)
          Some(Strategy.ReadEntityList(DomainEntityType(s.name)))
        else
          Some(Strategy.Skip)
      case None => pathname.getParent.map(x =>
        classes.getNode(x.v) match {
          case Some(s) => pathname.leaf match {
            case "_create_" => Strategy.CreateEntity(DomainEntityType(x.leaf))
            case x => Strategy.GetEntity(DomainEntityType(s.name), DomainObjectId(x))
          }
          case None => Strategy.Skip
        }
      )
    }
  }

  def getEntitySchema(entitytype: DomainEntityType): Option[Schema]

  def getEntity(
    entitytype: DomainEntityType,
    id: DomainObjectId
  ): Option[Consequence[Option[EntityDetailModel]]]

  def readEntityList(q: Query): Option[Consequence[EntityListModel]]

  def createEntity(
    entitytype: DomainEntityType,
    record: IRecord
  ): Option[Consequence[DomainObjectId]]

  def updateEntity(
    klass: DomainEntityType,
    id: DomainObjectId,
    data: IRecord
  ): Option[Consequence[Unit]]

  def deleteEntity(
    klass: DomainEntityType,
    id: DomainObjectId
  ): Option[Consequence[Unit]]
}

object DomainModelSpace {
  trait Factory {
    def parse(p: File): Option[DomainModel]
  }
  object Factory {
    val empty = new Factory() {
      def parse(p: File): Option[DomainModel] = None
    }
  }
}
