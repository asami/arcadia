package arcadia.scenario

import scala.util.control.NonFatal
import org.goldenport.exception.RAISE
import arcadia._
import arcadia.context._

/*
 * @since   Sep. 17, 2017
 * @version Sep. 20, 2017
 * @author  ASAMI, Tomoharu
 */
class ScenarioEngine(
  val platform: PlatformContext,
  val rule: ScenarioEngine.Rule
) {
  def apply(parcel: Parcel): Parcel = parcel.command.collect {
    case m: ScenarioCommand => _apply(parcel, m)
  }.getOrElse(parcel)

  private def _apply(parcel: Parcel, command: ScenarioCommand): Parcel = {
    val scenario = command.scenario
    Event.get(parcel).fold {
      RAISE.noReachDefect
    } { inevt =>
      try {
        val (updatedscenario, outevt) = scenario.apply(inevt)
        outevt.parcel
      } catch {
        case NonFatal(e) => parcel.goError(e)
      }
    }
  }
}

object ScenarioEngine {
  case class Rule()

  object Rule {
    val empty = Rule()
    def create(): Rule = Rule()
  }
}
