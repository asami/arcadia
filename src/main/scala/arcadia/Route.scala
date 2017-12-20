package arcadia

import arcadia._
import controller._
import model._

/*
 * @since   Dec. 20, 2017
 * @version Dec. 21, 2017
 * @author  ASAMI, Tomoharu
 */
case class Route(
  slots: Vector[Route.Slot]
) {
  def isAccept(parcel: Parcel): Boolean = slots.exists(_.isAccept(parcel))

  def apply(parcel: Parcel): Parcel =
    slots.find(_.isAccept(parcel)).map(_.apply(parcel)).getOrElse(parcel)

  def complement(rhs: Route): Route = copy(slots = slots ++ rhs.slots)
}

object Route {
  val empty = Route(Vector.empty)
  val system = Route(Vector(
    Slot(UnauthorizedGetGuard, UnauthorizedAccessAction),
    Slot(UnauthorizedMutationGuard, UnauthorizedUserAction),
    Slot(OperationOutcomeGuard, RedirectSinglePageAction())
  ))

  case class Slot(guard: Guard, action: Action) {
    def isAccept(parcel: Parcel) = guard.isAccept(parcel)
    def apply(parcel: Parcel) = action.apply(parcel)
  }

  case object UnauthorizedGetGuard extends Guard {
    def isAccept(p: Parcel) = p.command.fold(false) {
      case m: UnauthorizedCommand => m.isGet
      case _ => false
    }
  }

  case object UnauthorizedMutationGuard extends Guard {
    def isAccept(p: Parcel) = p.command.fold(false) {
      case m: UnauthorizedCommand => m.isMutation
      case _ => false
    }
  }

  case object OperationOutcomeGuard extends Guard {
    def isAccept(p: Parcel) = p.getEffectiveModel.map(_.isInstanceOf[OperationOutcomeModel]).getOrElse(false)
  }

  case object UnauthorizedAccessAction extends Action {
    protected def execute_Apply(p: Parcel) = p.command.map {
      case m: UnauthorizedCommand => m.command.
          map(x => p.withCommand(RerunCommand(p.withCommand(x)))).
          getOrElse(p)
      case _ => p
    }.getOrElse(p)
  }

  /*
   * error, redirect, page
   */
  case object UnauthorizedUserAction extends Action {
    // TODO
    protected def execute_Apply(p: Parcel) = p.withModel(ErrorModel.unauthorized(p))
  }
}
