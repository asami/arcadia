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
 *  version Mar. 30, 2023
 *  version Apr. 16, 2023
 *  version Jun. 24, 2023
 * @version Aug. 31, 2023
 * @author  ASAMI, abstraclass
 */
trait DomainModelSpace {
  import DomainModelSpace._

  val classes = Tree.create[DomainClass]()

  def isAvailableResource(pathname: PathName): Boolean =
    pathname.components match {
      case Nil => false
      case resource :: _ => classes.getNode(resource).isDefined
    }

  def strategy(parcel: Parcel, pathname: PathName): Option[Strategy] = {
    classes.getNode(pathname.v) match {
      case Some(s) =>
        if (s.isLeaf)
          Some(Strategy.ReadEntityList(DomainEntityType(s.name)))
        else
          Some(Strategy.Skip)
      case None => _strategy(pathname)
      // case None => pathname.getParent.map(resourcename =>
      //   classes.getNode(resourcename.v) match {
      //     case Some(resource) => pathname.leaf match {
      //       case "_create_" => Strategy.CreateEntity(DomainEntityType(resourcename.leaf))
      //       case "_update_" => Strategy.UpdateEntity(DomainEntityType(resourcename.leaf), ???)
      //       case "_delete_" => ???
      //       case x => Strategy.GetEntity(DomainEntityType(resource.name), DomainObjectId(x))
      //     }
      //     case None => Strategy.Skip
      //   }
      // )
    }
  }

  private def _strategy(pathname: PathName): Option[Strategy] = {
    pathname.components match {
      case Nil => None
      case resource :: "_create_" :: Nil => Some(Strategy.CreateEntity(DomainEntityType(resource)))
      case resource :: "_update_" :: Nil => Some(Strategy.UpdateEntity(DomainEntityType(resource), None))
      case resource :: "_delete_" :: Nil => Some(Strategy.DeleteEntity(DomainEntityType(resource), None))
      case resource :: id :: Nil => Some(Strategy.GetEntity(DomainEntityType(resource), DomainObjectId(id)))
      case resource :: id :: "_update_" :: Nil => Some(Strategy.UpdateEntity(DomainEntityType(resource), Some(DomainObjectId(id))))
      case resource :: id :: "_delete_" :: Nil => Some(Strategy.DeleteEntity(DomainEntityType(resource), Some(DomainObjectId(id))))
      case _ => None
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
