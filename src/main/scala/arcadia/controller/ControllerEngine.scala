package arcadia.controller

import org.goldenport.trace.Result
import arcadia._

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 *  version Sep. 17, 2017
 *  version Oct.  6, 2017
 * @version Nov. 13, 2017
 * @author  ASAMI, Tomoharu
 */
class ControllerEngine(
  applicationRule: ControllerEngine.Rule,
  extend: List[ControllerEngine],
  systemRule: ControllerEngine.Rule
) {
  def apply(parcel: Parcel): Parcel = parcel.executeWithTrace("ControllerEngine#apply", parcel.show) {
    val r = applyOption(parcel) getOrElse parcel
    Result(r, r.show)
  }

  def applyOption(parcel: Parcel): Option[Parcel] = parcel.executeWithTrace("ControllerEngine#applyOption", parcel.show) {
    val r = applyApplicationOption(parcel) orElse {
      systemRule.findController(parcel).map(_.apply(parcel))
    }
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

    def append(p: (Guard, Controller)): Rule = copy(slots = slots :+ Slot(p))
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
