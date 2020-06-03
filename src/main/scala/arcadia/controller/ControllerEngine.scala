package arcadia.controller

import org.goldenport.exception.RAISE
import org.goldenport.trace.Result
import arcadia._

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 *  version Sep. 17, 2017
 *  version Oct.  6, 2017
 *  version Nov. 13, 2017
 *  version Dec. 21, 2017
 *  version Jan.  7, 2018
 *  version Mar. 13, 2018
 * @version Mar. 23, 2020
 * @author  ASAMI, Tomoharu
 */
class ControllerEngine(
  prologueRule: ControllerEngine.Rule,
  applicationRule: ControllerEngine.Rule,
  extend: List[ControllerEngine],
  systemRule: ControllerEngine.Rule
) {
  def applyRerun(parcel: Parcel, count: Int): Parcel = {
    @annotation.tailrec
    def go(p: Parcel, count: Int): Parcel = {
      val r = apply(p)
      r.command match {
        case Some(s) => s match {
          case RerunCommand(x) =>
            val a = _invoke(x)
            if (count <= 0)
              RAISE.noReachDefect
            else
              go(a, count - 1)
          case _ => r
        }
        case None => r
      }
    }
    go(parcel, 1)
  }

  private def _invoke(p: Parcel): Parcel =
    if (InvokePlatformController.guard.isAccept(p))
      InvokePlatformController.apply(p)
    else if (InvokeOperationController.guard.isAccept(p))
      InvokeOperationController.apply(p)
    else
      p

  def apply(parcel: Parcel): Parcel = parcel.executeWithTrace("ControllerEngine#apply", parcel.show) {
    val r = applyOption(parcel) getOrElse parcel
    Result(r, r.show)
  }

  def applyOption(parcel: Parcel): Option[Parcel] = parcel.executeWithTrace("ControllerEngine#applyOption", parcel.show) {
    val r = prologueRule.findController(parcel).map(_.apply(parcel)).
      orElse(
        applyApplicationOption(parcel)
      ).orElse(
        systemRule.findController(parcel).map(_.apply(parcel))
      )
    Result(r, r.map(_.show).getOrElse(""))
  }

  def applyApplicationOption(parcel: Parcel): Option[Parcel] = parcel.executeWithTrace("ControllerEngine#applyApplicationOption", parcel.show) {
    val r = applicationRule.findController(parcel).map(_.apply(parcel)) orElse {
      extend.toStream.flatMap(_.applyOption(parcel)).headOption
    }
    Result(r, r.map(_.show).getOrElse(""))
  }
}

object ControllerEngine {
  case class Rule(slots: Vector[ControllerEngine.Slot]) {
    def findController(parcel: Parcel): Option[Controller] =
      slots.find(_.isAccept(parcel)).map(_.controller)

    def append(p: (Guard, Controller), ps: (Guard, Controller)*): Rule = copy(slots = (slots :+ Slot(p)) ++ ps.map(Slot(_)))
  }
  object Rule {
    val empty = Rule(Vector.empty)

    def create(slots: (Guard, Controller)*): Rule = Rule(slots.toVector.map(Slot(_)))
  }

  case class Slot(guard: Guard, controller: Controller) {
    def isAccept(parcel: Parcel): Boolean = guard.isAccept(parcel)
  }
  object Slot {
    def apply(p: (Guard, Controller)): Slot = Slot(p._1, p._2)
  }
}
