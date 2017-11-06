package arcadia

import scala.util.control.NonFatal
import org.goldenport.exception.RAISE
import org.goldenport.values.ResourceName
import org.goldenport.record.v2.Record
import arcadia.context._
import arcadia.controller.ControllerEngine
import arcadia.view.ViewEngine
import arcadia.controller._
import arcadia.scenario._

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 *  version Sep.  2, 2017
 *  version Oct. 27, 2017
 * @version Nov.  6, 2017
 * @author  ASAMI, Tomoharu
 */
class WebEngine(
  val platform: PlatformContext,
  val application: WebApplication,
  val extend: List[WebEngine]
) {
  val rule: WebApplicationRule = extend./:(application.config.toRule)(_ complement _.application.config.toRule)
  val view: ViewEngine = new ViewEngine(platform, application.view, extend.map(_.view))
  val scenariorule = ScenarioEngine.Rule.create() // TODO
  val scenario = new ScenarioEngine(platform, scenariorule)
  val systemcontroller = WebApplication.standardControllerRule.append(
    ScenarioController(scenario).gc
  )
  val controller: ControllerEngine = new ControllerEngine(
    application.controller,
    extend.map(_.controller),
    systemcontroller
  )

  def apply(p: Parcel): Content = try {
    val parcel =
      p.withApplicationRule(rule).withApplication(application)
    val a = controller.apply(parcel)
    a.content getOrElse {
      view.apply(a) match {
        case m: NotFoundContent => view.error(p, 404)
        case r =>
          if (r.expiresPeriod.isDefined)
            r
          else
            r.expiresKind.fold(r) { x =>
              application.config.getExpiresPeriod(x).fold(r)(r.withExpiresPeriod)
            }
      }
    }
  } catch {
    case NonFatal(e) => view.error(p, e)
  }

  def render(p: Parcel): String = {
    apply(p) match {
      case m: StringContent => m.string
      case m: BinaryContent => RAISE.noReachDefect
      case m: XmlContent => m.xml.toString
      case m: RedirectContent => RAISE.noReachDefect
      case m: NotFoundContent => RAISE.noReachDefect // TODO
    }
  }

  def shutdown(): Unit = view.shutdown()
}
object WebEngine {
}
