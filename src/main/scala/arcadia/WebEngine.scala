package arcadia

import scala.util.control.NonFatal
import org.goldenport.exception.RAISE
import org.goldenport.values.ResourceName
import org.goldenport.record.v2.Record
import org.goldenport.trace.{TraceContext, Result}
import arcadia.context._
import arcadia.controller.ControllerEngine
import arcadia.view.ViewEngine
import arcadia.controller._
import arcadia.model.ErrorModel
import arcadia.scenario._

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 *  version Sep.  2, 2017
 *  version Oct. 27, 2017
 *  version Nov. 16, 2017
 *  version Dec. 21, 2017
 *  version Jan.  8, 2018
 *  version Mar. 13, 2018
 *  version Apr. 30, 2019
 *  version May.  1, 2019
 * @version Jul. 20, 2019
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
  val prologuecontroller = {
    val route =
      if (false)
        Route.empty
      else
        Route.prologue
    ControllerEngine.Rule.create(RouterController(route).gc)
  }
  val systemcontroller = WebApplication.standardControllerRule.append(
    ScenarioController(scenario).gc,
    RouterController(rule.route).gc,
    RouterController(Route.system).gc
  )
  val controller: ControllerEngine = new ControllerEngine(
    prologuecontroller,
    application.controller,
    extend.map(_.controller),
    systemcontroller
  )
  val isTrace = true

  def apply(p: Parcel): Content = {
    val parcel0 =
      p.withApplicationRule(rule).withApplication(application)
    val parcel =
      if (isTrace)
        parcel0.withTrace(new TraceContext)
      else
        parcel0
    try {
      val r = parcel.executeWithTrace("WebEngine#apply", p.show) {
        val a: Parcel = controller.applyRerun(parcel, 1)
        val c: Content = a.content getOrElse {
          view.applyOption(a).map {
            case m: ErrorContent => m
            case m =>
              if (m.expiresPeriod.isDefined)
                m
              else
                m.expiresKind.fold(m) { x =>
                  application.config.getExpiresPeriod(x).fold(m)(m.withExpiresPeriod)
                }
          }.getOrElse {
            a.model.collect {
              case m: ErrorModel => ErrorModelContent(m)
            }.getOrElse(
              view.error(a, 404)
            )
          }
        }
        Result(c, c.show)
      }
      r match {
        case m: NotFoundContent => view.error(parcel, 404)
        case m: RedirectContent => m
        case ErrorModelContent(m) => view.error(parcel, m)
        case _ =>
          parcel.trace.fold(r)(x =>
            if (parcel.isShowTrace)
              //        r.addCallTree(x.showTreeSplit)
              r.addCallTree(x.showTree)
            else
              r
          )
      }
    } catch {
      case NonFatal(e) => view.error(parcel, e)
    }
  }

  def render(p: Parcel): String = {
    apply(p) match {
      case m: StringContent => m.string
      case m: BinaryContent => RAISE.noReachDefect
      case m: XmlContent => m.toHtmlString
      case m: RedirectContent => RAISE.noReachDefect
      case m: NotFoundContent => RAISE.noReachDefect // TODO
      case m: ErrorContent => RAISE.noReachDefect // TODO
    }
  }

  def shutdown(): Unit = view.shutdown()
}
object WebEngine {
}
