package arcadia

import arcadia.model._

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 * @version Sep. 21, 2017
 * @author  ASAMI, Tomoharu
 */
trait Guard {
  def isAccept(p: Parcel): Boolean
}

case class CommandGuard(classes: Vector[Class[_]]) extends Guard {
  def isAccept(p: Parcel) = p.command.fold(false)(c => classes.exists(_.isInstance(c)))
}
object CommandGuard {
  def apply(head: Class[_], tail: Class[_]*): CommandGuard = CommandGuard(
    head +: tail.toVector
  )
}

case class PathnameGuard(pathname: String) extends Guard {
  def isAccept(p: Parcel) = p.getOperationName == Some(pathname) // TODO
}

case class OperationNameGuard(pathname: String) extends Guard {
  def isAccept(p: Parcel) = p.getOperationName == Some(pathname)
}

case class ModelNameGuard(name: String) extends Guard {
  def isAccept(p: Parcel) = p.getEffectiveModel.fold(false)(x =>
    x.featureName == name || x.featureNameAliases.exists(_ == name))
}

case object DashboardModelGuard extends Guard {
  def isAccept(p: Parcel) = p.getEffectiveModel.fold(false)(_.isInstanceOf[IDashboardModel])
}

case object AllModelGuard extends Guard {
  def isAccept(p: Parcel) = p.getEffectiveModel.isDefined
}

object NotImplementedYetGuard extends Guard {
  def isAccept(p: Parcel): Boolean = false
}