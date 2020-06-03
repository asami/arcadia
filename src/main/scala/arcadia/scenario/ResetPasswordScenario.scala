package arcadia.scenario

import java.util.Locale
import java.net.URI
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.values.PathName
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => _, Schema}
import org.goldenport.record.v2.{Invalid, Conclusion}
import org.goldenport.record.v2.util.{RecordUtils, SchemaBuilder}
import arcadia._
import arcadia.context._
import arcadia.model._
import arcadia.model.FormModel.Field
import arcadia.view._
import arcadia.ScenarioCommand.{PROP_SUBMIT, PROP_SCENARIO}
import arcadia.controller._
import arcadia.rule.ResetPasswordRule

/*
 * @since   May. 26, 2020
 *  version May. 29, 2020
 * @version Jun.  2, 2020
 * @author  ASAMI, Tomoharu
 */
case class ResetPasswordScenario(
  state: State,
  data: IRecord,
  token: Field,
  password: Field,
  confirmPassword: Field,
  execute: Field,
  move: Move
) extends Scenario {
  import ResetPasswordScenario._

  val scenarioClass = ResetPasswordScenario

  val stateMachine = new StateMachine {
    import StateMachine.Slot
    val slots = Vector(
      Slot(InputState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, CancelAction),
        Transition(OkEventGuard, ExecuteAction(execute)),
        Transition(ExecuteEventGuard, ExecuteAction(execute))
      )),
      Slot(ShowState, Transitions(
        Transition(AllGuard, EndAction)
      )),
      Slot(EndState, Transitions(
        Transition(AllGuard, ReturnAction)
      ))
    )
  }

  override val schema = {
    Schema(Vector(
      token.toColumn,
      password.toColumn,
      confirmPassword.toColumn
    ))
    // import SchemaBuilder._
    // SchemaBuilder.create(
    //   // CL(PROP_TOKEN, token.label getOrElse "Token"),
    //   // CL(PROP_PASSWORD, password.label getOrElse "Password"),
    //   // CL(PROP_PASSWORD_CONFIRM, confirmPassword.label getOrElse "Confirm Password")
    // )
  }

  private def _execute_label = execute.label getOrElse "Reset"

  private def inputForm(p: Intent) = {
    // val ph = p.context.map { ctx =>
    //   Record.data(
    //     PROP_PASSWORD -> ctx.resetPasswordRule.passwordRule.placeholder.as(ctx.locale)
    //   )
    // }.getOrElse(Record.empty)
    PropertyInputFormModel(
      new URI(""),
      Post,
      schema,
      data,
      Hiddens.empty,
      Submits(Submit(ExecuteSubmitKind, I18NString(_execute_label)))
    )
  }

  def withState(p: State) = copy(state = p)
  def withData(p: IRecord) = copy(data = p)

  def setup(locale: Locale, rule: ResetPasswordRule): ResetPasswordScenario =
    if (password.placeholder.isEmpty) {
      copy(password = password.withPlaceholder(rule.passwordRule.placeholder.as(locale)))
    } else {
      this
    }

  override protected def adjust_Intent(p: Intent): Intent = p

  def execute(p: Intent): Intent = {
    p.context.map { ctx =>
      (data.getString(PROP_TOKEN), data.getString(PROP_PASSWORD), data.getString(PROP_PASSWORD_CONFIRM)) match {
        case (Some(token), Some(password), Some(confirmpassword)) =>
          ctx.resetPassword(token, password, Some(confirmpassword)) match {
            case Right(_) => _response(p)
            case Left(c) => _error(p, c)
          }
        case (u, pw, pwc) =>
          val xs = Vector(
            if (u.isEmpty) Some(PROP_TOKEN) else None,
            if (pw.isEmpty) Some(PROP_PASSWORD) else None,
            if (pwc.isEmpty) Some(PROP_PASSWORD_CONFIRM) else None
          )
          _error(p, Conclusion.missings(xs.flatten))
      }
    }.getOrElse(_go_error(p, "Missing scenario execution context for ResetPassword"))
  }

  private def _response(p: Intent): Intent = move.success(p)

  private def _error(p: Intent, c: Conclusion): Intent = {
    val model = inputForm(p).setError(c)
    p.withModel(model)
  }

  private def _go_error(p: Intent, msg: String): Intent = {
    val a = p.goError(msg)
    // errorView.map(a.withViewCommand) getOrElse a
    move.error(a)
  }

  def marshall = Record.data(
    "name" -> ResetPasswordScenario.name,
    "state" -> state.marshall,
    "data" -> data
  ).toJsonString
}
object ResetPasswordScenario extends ScenarioClass {
  val PROP_TOKEN = "token"
  val PROP_PASSWORD = "password"
  val PROP_PASSWORD_CONFIRM = "password_confirm"

  def launch(p: Parcel, action: ResetPasswordScenarioAction): Parcel = {
    implicit val strategy = p.toStrategy
    val data = p.inputQueryFormParameters
    val rule = p.context.map(_.resetPasswordRule) getOrElse ResetPasswordRule.default
    data.getString(PROP_SCENARIO).flatMap(unmarshallOption(strategy, action, _)).
      map(_go(p, rule, _, data)).
      getOrElse(_start(p, rule, action, data))
  }

  private def _start(
    p: Parcel,
    rule: ResetPasswordRule,
    action: ResetPasswordScenarioAction,
    data: IRecord
  ): Parcel = {
    val locale = p.locale
    val pathname = _pathname(p)
    val parcel = p.withUsageKind(InvokeUsage) // ???
    val token = Field.hidden(PROP_TOKEN, data.toRecord.takeString(PROP_TOKEN))
    val password = Field.password(PROP_PASSWORD, action.passwordLabel.map(_.toString(locale))).
      withPlaceholder(rule.passwordRule.placeholder.as(locale))
    val confirmPassword = Field.password(PROP_PASSWORD_CONFIRM, action.passwordConfirmLabel.map(_.toString(locale)))
    val execute = Field.submit(action.executeLabel.map(_.toString(locale)))
    val move = Move.success2error2(action.successRedirect, action.successView, None, action.errorView)
    val scenario = ResetPasswordScenario(
      InputState,
      data,
      // action.usernameLabel.map(_.toString(locale)),
      // action.passwordLabel.map(_.toString(locale)),
      // action.newPasswordLabel.map(_.toString(locale)),
      // action.executeLabel.map(_.toString(locale)),
      // action.successRedirect,
      // action.successView,
      // action.errorView,
      token,
      password,
      confirmPassword,
      execute,
      move
    )
    val cmd = ScenarioCommand(scenario, pathname, StartEvent(p, data))
    parcel.withCommand(cmd)
  }

  private def _go(
    p: Parcel,
    rule: ResetPasswordRule,
    scenario: ResetPasswordScenario,
    data: IRecord
  ): Parcel = data.getString(PROP_SUBMIT).map(x =>
    Event.get(p, x).
      map { event =>
        val locale = p.locale
        val pathname = _pathname(p)
        val s = scenario.withData(data).setup(locale, rule)
        val cmd = ScenarioCommand(s, pathname, event)
        p.withCommand(cmd)
      }.getOrElse(RAISE.notImplementedYetDefect) // TODO WebScenarioDefect)
  ).getOrElse(
    RAISE.notImplementedYetDefect // TODO WebScenarioDefect
  )

  private def _pathname(p: Parcel): PathName = p.command.collect {
    case MaterialCommand(pn) => pn
  }.getOrElse(RAISE.noReachDefect)

  def unmarshallOption(p: String): Option[ResetPasswordScenario] = RAISE.unsupportedOperationFault

  def unmarshallOption(
    strategy: RenderStrategy,
    action: ResetPasswordScenarioAction,
    p: String
  ): Option[ResetPasswordScenario] =
    if (p.startsWith("{"))
      _unmarshall_option(strategy, action, p)
    else
      None

  private def _unmarshall_option(
    strategy: RenderStrategy,
    action: ResetPasswordScenarioAction,
    p: String
  ): Option[ResetPasswordScenario] = {
    val locale = strategy.locale
    val json = Json.parse(p)
    (json \ "name").asOpt[String].map { name =>
      val state = State.unmarshall((json \ "state").as[String])
      val data = Record.create((json \ "data").get)
      val token = Field.hidden(PROP_TOKEN,  data.toRecord.takeString(PROP_TOKEN))
      val password = Field.password(PROP_PASSWORD, action.passwordLabel.map(_.toString(locale)))
      val confirmPassword = Field.password(PROP_PASSWORD_CONFIRM, action.passwordConfirmLabel.map(_.toString(locale)))
      val execute = Field.submit(action.executeLabel.map(_.toString(locale)))
      val move = Move.success2error2(action.successRedirect, action.successView, None, action.errorView)
      ResetPasswordScenario(
        state,
        data,
        // action.usernameLabel.map(_.toString(locale)),
        // action.passwordLabel.map(_.toString(locale)),
        // action.newPasswordLabel.map(_.toString(locale)),
        // action.executeLabel.map(_.toString(locale)),
        // action.successRedirect,
        // action.successView,
        // action.errorView
        token,
        password,
        confirmPassword,
        execute,
        move
      )
    }
  }
}
