package arcadia

import org.goldenport.values.PathName
import com.asamioffice.goldenport.text.UPathString
import arcadia.model._

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 *  version Sep. 23, 2017
 *  version Oct.  8, 2017
 *  version Nov.  5, 2017
 * @version Jul. 21, 2019
 * @author  ASAMI, Tomoharu
 */
trait Guard {
  def isAccept(p: Parcel): Boolean

  protected final def is_pathname_command(p: Parcel): Boolean =
    p.command.map {
      case m: MaterialCommand => true
      case _ => false
    } getOrElse(false)

  protected final def execute_pathname(p: Parcel)(body: PathName => Parcel): Parcel =
    p.command.map {
      case MaterialCommand(pathname) => body(pathname)
      case _ => p
    } getOrElse(p)
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
  val operationName = UPathString.getPathnameBody(pathname)
  def isAccept(p: Parcel) = p.getOperationName.fold(false)(op =>
    op == pathname || op == operationName
  )
}

case class OperationNameGuard(pathname: String) extends Guard {
  def isAccept(p: Parcel) = p.isOperationPathName(pathname)
}

case class ModelNameGuard(name: String) extends Guard {
  def isAccept(p: Parcel) = p.getEffectiveModel.fold(false)(x =>
    x.featureName == name || x.featureNameAliases.exists(_ == name))
}

case object DashboardModelGuard extends Guard {
  def isAccept(p: Parcel) = p.getEffectiveModel.fold(false)(_.isInstanceOf[IDashboardModel])
}

case class AndGuard(guards: Vector[Guard]) extends Guard {
  def isAccept(p: Parcel) = guards.forall(_.isAccept(p))
}
object AndGuard {
  def apply(p: Guard, ps: Guard*): AndGuard = AndGuard((p +: ps).toVector)
}

case class OrGuard(guards: Vector[Guard]) extends Guard {
  def isAccept(p: Parcel) = guards.exists(_.isAccept(p))
}
object OrGuard {
  def apply(p: Guard, ps: Guard*): OrGuard = OrGuard((p +: ps).toVector)
}

case object IndexGuard extends Guard {
  def isAccept(p: Parcel) = (
    p.command.fold(false)(_.isInstanceOf[IndexCommand]) ||
      p.isOperationPathName("index")
  )
}

case object AllModelGuard extends Guard {
  def isAccept(p: Parcel) = p.getEffectiveModel.isDefined
}

case object NoneGuard extends Guard {
  def isAccept(p: Parcel): Boolean = false
}

case object NotImplementedYetGuard extends Guard {
  def isAccept(p: Parcel): Boolean = false
}
