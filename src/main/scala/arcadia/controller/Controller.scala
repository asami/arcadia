package arcadia.controller

import scala.util.control.NonFatal
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.trace.Result
import arcadia._
import arcadia.scenario.ScenarioEngine

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 *  version Sep. 17, 2017
 *  version Oct. 14, 2017
 *  version Nov. 13, 2017
 *  version Dec. 21, 2017
 * @version Jan. 14, 2018
 * @author  ASAMI, Tomoharu
 */
abstract class Controller(rule: Controller.Rule) {
  def apply(parcel: Parcel): Parcel = parcel.executeWithTrace(s"${getClass.getSimpleName}#apply", parcel.show) {
    val a = prologue_Apply(parcel)
    val b = rule.apply(a)
    val r = epilogue_Apply(b)
    Result(r, r.show)
  }

  def guard: Guard = NoneGuard
  def gc: (Guard, Controller) = _gc
  private lazy val _gc: (Guard, Controller) = (guard, this)

  protected def prologue_Apply(parcel: Parcel): Parcel = parcel
  protected def epilogue_Apply(parcel: Parcel): Parcel = parcel
}
object Controller {
  val PROP_REDIRECT = "web.redirect"

  case class Rule(actions: List[Action]) {
    def apply(parcel: Parcel): Parcel = actions./:(parcel)((z, x) => x apply z)
  }
}

case object ResourceDetailController extends Controller(
  Controller.Rule(List(ResourceDetailAction()))
) {
  override val guard = new Guard {
    def isAccept(p: Parcel): Boolean = is_pathname_command(p)
  }
}

case object ResourceListController extends Controller(
  Controller.Rule(Nil)
) {
  override val guard = NotImplementedYetGuard
}

case object IndexController extends Controller(
  Controller.Rule(List(IndexAction()))
) {
  override val guard = CommandGuard(classOf[IndexCommand])
}

case class ScenarioController(engine: ScenarioEngine) extends Controller(
  Controller.Rule(List(ScenarioAction(engine)))
) {
  override val guard = CommandGuard(classOf[ScenarioCommand])
}

case class OperationController(rule: OperationController.Rule) extends Controller(
  Controller.Rule(rule.actions)
) {
  override val guard = rule.guard
}
object OperationController {
  def get(operation: String, json: JsValue): Option[OperationController] = try {
    Some(OperationController(parse(operation, json)))
  } catch {
    case NonFatal(e) => None
  }

  case class Rule(
    operation: String,
    actions: List[Action]
  ) {
    def guard: Guard = OperationNameGuard(operation)
  }

  def parse(operation: String, json: JsValue): Rule = {
    json match {
      case JsArray(xs) => Rule(operation, xs.toList.map(_to_action))
      case m: JsObject => Rule(operation, List(_object_to_action(m)))
      case m => RAISE.notImplementedYetDefect
    }
  }

  private def _to_action(p: JsValue): Action = p match {
    case m: JsObject => _object_to_action(m)
    case _ => BrokenAction("Not js object", p)
  }

  private def _object_to_action(p: JsObject): Action = Action.toAction(p)
}

case object LoginController extends Controller(
  Controller.Rule(List(LoginAction()))
) {
  override val guard = CommandGuard(classOf[LoginCommand])
}

case object LogoutController extends Controller(
  Controller.Rule(List(LogoutAction()))
) {
  override val guard = CommandGuard(classOf[LogoutCommand])
}

case class RouterController(route: Route) extends Controller(
  Controller.Rule(List(RouterAction(route)))
) {
  override val guard = new Guard {
    def isAccept(p: Parcel) = route.isAccept(p)
  }
}

// object RedirectController extends Controller(
//   Controller.Rule(List(RedirectAction()))
// ) {
//   override val guard = new Guard {
//     def isAccept(p: Parcel) = p.getEffectiveModel.fold(false) {
//       case m: arcadia.model.OperationOutcomeModel => true
//       case _ => false
//     }
//   }
// }

case object InvokeController extends Controller(
  Controller.Rule(List(InvokeAction()))
) {
  override val guard = CommandGuard(classOf[InvokeCommand])
}
