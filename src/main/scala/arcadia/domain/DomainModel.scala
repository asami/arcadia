package arcadia.domain

import java.io.File
import org.goldenport.tree.Tree
import org.goldenport.values.PathName
import arcadia._
import arcadia.context._

/*
 * @since   Dec.  4, 2022
 * @version Dec. 30, 2022
 * @author  ASAMI, Tomoharu
 */
class DomainModel() {
  import DomainModel._

  val classes = Tree.create[DomainClass]()

  def +(rhs: DomainModel): DomainModel = {
    classes.copyIn(rhs.classes)
    this
  }

  def isAvailableResource(pathname: PathName): Boolean =
    classes.getNode(pathname.v) match {
      case Some(s) => s.isLeaf
      case None => pathname.getParent.fold(false)(x => classes.getNode(x.v).isDefined)
    }

  def strategy(parcel: Parcel, pathname: PathName): Strategy = {
    classes.getNode(pathname.v) match {
      case Some(s) =>
        if (s.isLeaf)
          Strategy.ReadEntityList(s.name)
        else
          Strategy.Skip
      case None => pathname.getParent.map(x =>
        classes.getNode(x.v) match {
          case Some(s) => Strategy.GetEntity(s.name, pathname.leaf)
          case None => Strategy.Skip
        }
      ).getOrElse(Strategy.Skip)
    }
  }
}

object DomainModel {
  val empty = new DomainModel()

  trait Factory {
    def parse(p: File): Option[DomainModel]
  }
  object Factory {
    val empty = new Factory() {
      def parse(p: File): Option[DomainModel] = None
    }
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
}
