package arcadia.controller

import arcadia._
import arcadia.scenario.ScenarioEngine

/*
 * @since   Jul. 15, 2017
 *  version Jul. 16, 2017
 *  version Aug. 29, 2017
 * @version Sep. 17, 2017
 * @author  ASAMI, Tomoharu
 */
abstract class Controller(rule: Controller.Rule) {
  def apply(parcel: Parcel): Parcel = {
    val a = prologue_Apply(parcel)
    val b = rule.apply(parcel)
    epilogue_Apply(b)
  }

  protected def prologue_Apply(parcel: Parcel): Parcel = parcel
  protected def epilogue_Apply(parcel: Parcel): Parcel = parcel
}
object Controller {
  case class Rule(actions: List[Action]) {
    def apply(parcel: Parcel): Parcel = actions./:(parcel)((z, x) => x apply z)
  }
}

case object ResourceDetailController extends Controller(
  Controller.Rule(Nil)
) {
  val guard = NotImplementedYetGuard
  lazy val gc: (Guard, Controller) = (guard, this)
}

case object ResourceListController extends Controller(
  Controller.Rule(Nil)
) {
  val guard = NotImplementedYetGuard
  lazy val gc: (Guard, Controller) = (guard, this)
}

case object IndexController extends Controller(
  Controller.Rule(List(IndexAction()))
) {
  val guard = CommandGuard(classOf[IndexCommand])
  lazy val gc: (Guard, Controller) = (guard, this)
}

case class ScenarioController(engine: ScenarioEngine) extends Controller(
  Controller.Rule(List(ScenarioAction(engine)))
) {
  val guard = CommandGuard(classOf[ScenarioCommand])
  lazy val gc: (Guard, Controller) = (guard, this)
}
