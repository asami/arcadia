package arcadia.scenario

import scala.util.control.NonFatal
import org.goldenport.exception.RAISE
import arcadia._
import arcadia.context._

/*
 * @since   Sep. 17, 2017
 *  version Oct. 25, 2017
 *  version Nov. 16, 2017
 * @version Mar. 23, 2020
 * @author  ASAMI, Tomoharu
 */
class ScenarioEngine(
  val platform: PlatformContext,
  val rule: ScenarioEngine.Rule
) {
  // for scenario endpoint.
  def apply(parcel: Parcel): Parcel = parcel.command.collect {
    case m: ScenarioCandidateCommand => _apply(parcel, m)
    case m: ScenarioCommand => _apply(parcel, m)
  }.getOrElse(parcel)

  private def _apply(parcel: Parcel, command: ScenarioCandidateCommand): Parcel =
    command.getScenario.fold {
      Scenario.launch(parcel, command).map(_.start(parcel)).getOrElse {
        parcel.goNotFound(s"Unkown scenario: ${command.name}")
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

  private def _apply(p: Parcel, cmd: ScenarioCommand): Parcel =
    cmd.event match {
      case m: StartEvent => _start(p, cmd, m)
      case m => _go(p, cmd, m)
    }

  private def _start(p: Parcel, cmd: ScenarioCommand, evt: StartEvent): Parcel = {
    val vcmd = ViewCommand(cmd.pathname)
    val r = InputAction.model(p, cmd.scenario, cmd.scenario.schema, evt.data)
    r.withCommand(vcmd)
  }

  private def _go(p: Parcel, cmd: ScenarioCommand, evt: Event): Parcel = {
    try {
      cmd.scenario.execute(evt)
    } catch {
      case NonFatal(e) => p.goError(e)
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
