package arcadia.controller

import arcadia._

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 * @version Sep. 17, 2017
 * @author  ASAMI, Tomoharu
 */
class ControllerEngine(
  rule: ControllerEngine.Rule,
  extend: List[ControllerEngine]
) {
  def apply(parcel: Parcel): Parcel =
    applyOption(parcel) getOrElse parcel

  def applyOption(parcel: Parcel): Option[Parcel] =
    rule.findController(parcel).map(_.apply(parcel)) orElse {
      extend.toStream.flatMap(_.applyOption(parcel)).headOption
    }
}

object ControllerEngine {
  case class Rule(slots: Vector[ControllerEngine.Slot]) {
    def findController(parcel: Parcel): Option[Controller] =
      slots.find(_.isAccept(parcel)).map(_.controller)

    def append(p: (Guard, Controller)): Rule = copy(slots = slots :+ Slot(p))
  }
  object Rule {
    def create(slots: (Guard, Controller)*): Rule = Rule(slots.toVector.map(Slot(_)))
  }

  case class Slot(guard: Guard, controller: Controller) {
    def isAccept(parcel: Parcel): Boolean = guard.isAccept(parcel)
  }
  object Slot {
    def apply(p: (Guard, Controller)): Slot = Slot(p._1, p._2)
  }
}
