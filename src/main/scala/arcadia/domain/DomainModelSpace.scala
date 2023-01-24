package arcadia.domain

import java.io.File
import org.goldenport.context.Consequence
import org.goldenport.tree.Tree
import org.goldenport.values.PathName
import arcadia._
import arcadia.context._
import arcadia.model._
import arcadia.domain.DomainModel.Strategy

/*
 * @since   Jan.  1, 2023
 * @version Jan.  1, 2023
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
          Some(Strategy.ReadEntityList(s.name))
        else
          Some(Strategy.Skip)
      case None => pathname.getParent.map(x =>
        classes.getNode(x.v) match {
          case Some(s) => Strategy.GetEntity(s.name, pathname.leaf)
          case None => Strategy.Skip
        }
      )
    }
  }

  def getEntity(
    entitytype: DomainEntityType,
    id: DomainObjectId
  ): Option[Consequence[Option[EntityDetailModel]]]

  def readEntityList(q: Query): Option[Consequence[EntityListModel]]
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
