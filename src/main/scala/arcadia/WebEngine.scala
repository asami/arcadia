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
 * @version Nov. 16, 2017
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
        val a = controller.apply(parcel)
        val c = a.content getOrElse {
          a.model.flatMap {
            case m: ErrorModel => Some(ErrorModelContent(m))
            case _ => None
          }.getOrElse {
            view.apply(a) match {
              case m: ErrorContent => m
              case m =>
                if (m.expiresPeriod.isDefined)
                  m
                else
                  m.expiresKind.fold(m) { x =>
                    application.config.getExpiresPeriod(x).fold(m)(m.withExpiresPeriod)
                  }
            }
          }
        }
        Result(c, c.show)
      }
      r match {
        case m: NotFoundContent => view.error(parcel, 404)
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
      case m: XmlContent => m.xml.toString
      case m: RedirectContent => RAISE.noReachDefect
      case m: NotFoundContent => RAISE.noReachDefect // TODO
      case m: ErrorContent => RAISE.noReachDefect // TODO
    }
  }

  def shutdown(): Unit = view.shutdown()
}
object WebEngine {
}
