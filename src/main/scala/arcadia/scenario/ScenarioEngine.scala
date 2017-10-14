package arcadia.scenario

import scala.util.control.NonFatal
import org.goldenport.exception.RAISE
import arcadia._
import arcadia.context._

/*
 * @since   Sep. 17, 2017
 * @version Oct.  5, 2017
 * @author  ASAMI, Tomoharu
 */
class ScenarioEngine(
  val platform: PlatformContext,
  val rule: ScenarioEngine.Rule
) {
  def apply(parcel: Parcel): Parcel = parcel.command.collect {
    case m: ScenarioCommand => _apply(parcel, m)
  }.getOrElse(parcel)

  private def _apply(parcel: Parcel, command: ScenarioCommand): Parcel =
    command.getScenario.fold {
      Scenario.get(parcel, command).map(_.start(parcel)).getOrElse {
        RAISE.notImplementedYetDefect
      }
    } { scenario =>
      Event.get(parcel, command).fold {
        RAISE.noReachDefect
      } { inevt =>
        try {
          // val (updatedscenario, outevt) = scenario.apply(inevt)
          // outevt.parcel
          scenario.execute(inevt)
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
