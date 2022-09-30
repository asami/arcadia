package arcadia

import scala.util.control.NonFatal
import org.goldenport.exception.RAISE
import org.goldenport.values.ResourceName
import org.goldenport.record.v2.Record
import org.goldenport.trace.{TraceContext, Result}
import arcadia.context._
import arcadia.controller.ControllerEngine
import arcadia.view.ViewEngine
import arcadia.view.TemplateEngineHangar
import arcadia.view.ScalateTemplateEngine
import arcadia.controller._
import arcadia.model.ErrorModel
import arcadia.scenario._
import arcadia.service.ServiceFacility

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
 *  version Jul. 20, 2019
 *  version Mar. 31, 2020
 *  version Apr.  1, 2020
 *  version May.  8, 2020
 *  version Mar. 20, 2022
 * @version Sep. 10, 2022
 * @author  ASAMI, Tomoharu
 */
class WebEngine(
  val platform: PlatformContext,
  val services: ServiceFacility,
  val application: WebApplication,
  val extend: List[WebEngine],
  val config: WebApplicationConfig = WebApplicationConfig.empty,
  val webConfig: WebEngine.Config = WebEngine.Config.empty
) {
  val rule: WebApplicationRule = extend./:(application.config.toRule)(_ complement _.application.config.toRule).complement(config.toRule)
  val templateengines = {
    val a = webConfig.templateEngineHangarFactory.create(platform)
    val b = TemplateEngineHangar(new ScalateTemplateEngine(platform))
    a + b
  }
  val view: ViewEngine = new ViewEngine(
    platform,
    application.view,
    extend.map(_.view),
    templateengines
  )
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
    InvokePlatformController.gc,
    InvokeOperationController.gc,
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
    val parcel0 = p.complementApplicationRule(rule).
      withExecutionContext(services, application)
    val parcel1 = _normalize_auth(parcel0)
    val parcel =
      if (isTrace)
        parcel1.withTrace(new TraceContext)
      else
        parcel1
    try {
      val r = parcel.executeWithTrace("WebEngine#apply", p.show) {
        val aftercontroller: Parcel = controller.applyRerun(parcel, 1)
        val afterpage: Parcel = _normalize_page(aftercontroller)
        val c: Content = afterpage.content getOrElse {
          view.applyOption(afterpage).map {
            case m: ErrorContent => m
            case m =>
              if (m.expiresPeriod.isDefined)
                m
              else
                m.expiresKind.fold(m) { x =>
                  application.config.getExpiresPeriod(x).fold(m)(m.withExpiresPeriod)
                }
          }.getOrElse {
            afterpage.model.collect {
              case m: ErrorModel => ErrorModelContent(m)
            }.getOrElse(
              view.error(afterpage, 404)
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

  private def _normalize_auth(p: Parcel) =
    p.command.collect {
      case UnauthorizedCommand(req, Some(cmd)) => p.withCommand(cmd) // TODO security
    }.getOrElse(p)

  private def _normalize_page(p: Parcel) =
    p.command.collect {
      case ViewCommand(pathname) => p
    }.getOrElse(p)

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
  case class Config(
    templateEngineHangarFactory: TemplateEngineHangar.Factory = TemplateEngineHangar.Factory.empty
  )
  object Config {
    val empty = Config()
  }
}
