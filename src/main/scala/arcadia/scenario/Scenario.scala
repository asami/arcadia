package arcadia.scenario

import java.net.URI
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.value.{NamedValueInstance, EnumerationClass}
import org.goldenport.values.PathName
import org.goldenport.i18n.I18NString
import org.goldenport.io.UriUtils
import org.goldenport.util.StringUtils
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => _, Schema}
import org.goldenport.record.v2.{Invalid, Conclusion}
import org.goldenport.record.v2.util.{RecordUtils, SchemaBuilder}
import arcadia._
import arcadia.context._
import arcadia.model._
import arcadia.view._
import arcadia.domain._
import arcadia.ScenarioCommand.{PROP_SUBMIT, PROP_SCENARIO}
import arcadia.controller._

/*
 * @since   Sep. 16, 2017
 *  version Oct. 25, 2017
 *  version Nov. 16, 2017
 *  version Jan.  8, 2018
 *  version Apr.  8, 2018
 *  version Jul. 23, 2018
 *  version Sep.  1, 2018
 *  version Nov.  7, 2018
 *  version Jul. 29, 2019
 *  version Apr. 20, 2020
 *  version May. 28, 2020
 *  version Jun.  1, 2020
 *  version Mar. 30, 2022
 *  version Sep. 25, 2022
 * @version Nov. 28, 2022
 * @author  ASAMI, Tomoharu
 */
trait Scenario {
  def scenarioClass: ScenarioClass
  def scenarioName = scenarioClass.name
  def state: State
  def stateMachine: StateMachine
  def method: Method = Post // state.method
  def schema: Schema = getSchema getOrElse RAISE.noReachDefect // ScenarioDefect
  def getSchema: Option[Schema] = None
  def getCallerUri: Option[URI] = None
  def getCancelContent: Option[Content] = None
  def start(parcel: Parcel): Parcel = RAISE.unsupportedOperationFault
  def execute(evt: Event): Parcel = apply(evt)._2.parcel
  def apply(evt: Event): (Scenario, Event) = {
    val i = Intent(evt, this, state)
    val i1 = adjust_Intent(i)
    val r = stateMachine.apply(i1)
    r.toEvent
  }
  def withState(p: State): Scenario
  def marshall: String

  protected def adjust_Intent(p: Intent): Intent = p
}
object Scenario {
  val scenarios = Vector(
    CreateEntityScenario,
    UpdateEntityScenario,
    DeleteEntityScenario
  )
  val scenarios_stream = scenarios.toStream

  def launch(parcel: Parcel, cmd: ScenarioCandidateCommand): Option[Scenario] =
    scenarios_stream.flatMap(_.launch(parcel, cmd)).headOption

  def unmarshall(p: String): Scenario = unmarshallOption(p) getOrElse {
    RAISE.noReachDefect
  }

  def unmarshallOption(p: String): Option[Scenario] = scenarios.toStream.flatMap(_.unmarshallOption(p)).headOption
}

trait ScenarioClass {
  lazy val _name: String = StringUtils.classNameToHypenName("Scenario", this)
  def name: String = _name
  // launch via scenario endopoint.
  def launch(parcel: Parcel, cmd: ScenarioCandidateCommand): Option[Scenario] = RAISE.unsupportedOperationFault
  def unmarshallOption(p: String): Option[Scenario]
}

case class CreateEntityScenario(
  state: State,
  entityType: DomainEntityType,
  override val schema: Schema,
  data: IRecord
) extends Scenario {
  val scenarioClass = CreateEntityScenario
  override def getSchema = Some(schema)
  val stateMachine = CreateEntityScenario.stateMachine
  def withState(p: State) = copy(state = p)
  override def start(parcel: Parcel): Parcel = CreateEntityScenario.start(parcel, entityType, schema, data)

  override protected def adjust_Intent(p: Intent): Intent =
    p.withDomainEntityType(entityType)

  def marshall = Record.data(
    "name" -> CreateEntityScenario.name,
    "state" -> state.marshall,
    "entity" -> entityType.v,
    "schema" -> schema.toMarshalizable.marshallRecord,
    "data" -> data
  ).toJsonString
}
object CreateEntityScenario extends ScenarioClass {
  val stateMachine = new StateMachine {
    import StateMachine.Slot
    val slots = Vector(
      Slot(InputState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, CancelAction),
        Transition(InputEventGuard, ValidationAction),
        Transition(OkEventGuard, EntityCreateAction),
        Transition(CreateEventGuard, EntityCreateAction)
      )),
      Slot(ConfirmState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, InputAction),
        Transition(OkEventGuard, ShowAction)
      )),
      Slot(ShowState, Transitions(
        Transition(AllGuard, EndAction)
      )),
      Slot(EndState, Transitions(
        Transition(AllGuard, ReturnAction)
      ))
    )
  }

  override def launch(p: Parcel, cmd: ScenarioCandidateCommand): Option[Scenario] = {
    val parcel = p.withUsageKind(CreateUsage)
    def entitytype = cmd.entityType
    def resolveschema(p: Schema): Schema =
      parcel.render.map(_.resolveSchema(entitytype, p)).getOrElse(p)
    if (cmd.name == name) {
      def data = cmd.formRecord
      parcel.context.flatMap(_.
        getEntitySchema(cmd.entityName).map(x => _init(entitytype, resolveschema(x), data)))
    } else {
      None
    }
  }

  private def _init(entity: DomainEntityType, schema: Schema, data: IRecord): CreateEntityScenario =
    CreateEntityScenario(InitState, entity, schema, data)

  protected[scenario] def start(p: Parcel, entity: DomainEntityType, schema: Schema, data: IRecord): Parcel = {
    val parcel = p.withUsageKind(CreateUsage)
    val scenario = CreateEntityScenario(InputState, entity, schema, data)
    InputAction.model(parcel, scenario, schema, data)
  }

  def unmarshallOption(p: String): Option[Scenario] =
    if (p.startsWith("{"))
      _unmarshall_option(p)
    else
      None

  private def _unmarshall_option(p: String): Option[Scenario] = {
    val json = Json.parse(p)
    (json \ "name").asOpt[String].map { name =>
      val state = State.unmarshall((json \ "state").as[String])
      val entity = DomainEntityType((json \ "entity").as[String])
      val schema = Schema.json.unmarshall(json \ "schema")
      val data = Record.create(json \ "data")
      CreateEntityScenario(state, entity, schema, data)
    }
  }
}

case class UpdateEntityScenario(
  state: State,
  entityType: DomainEntityType,
  override val schema: Schema,
  data: IRecord
) extends Scenario {
  val scenarioClass = UpdateEntityScenario
  override def getSchema = Some(schema)
  val stateMachine = UpdateEntityScenario.stateMachine
  def withState(p: State) = copy(state = p)
  override def start(parcel: Parcel): Parcel = UpdateEntityScenario.start(parcel, entityType, schema, data)

  override protected def adjust_Intent(p: Intent): Intent =
    p.withDomainEntityType(entityType)

  def marshall = Record.data(
    "name" -> UpdateEntityScenario.name,
    "state" -> state.marshall,
    "entity" -> entityType.v,
    "schema" -> schema.toMarshalizable.marshallRecord,
    "data" -> data
  ).toJsonString
}
object UpdateEntityScenario extends ScenarioClass {
  val stateMachine = new StateMachine {
    import StateMachine.Slot
    val slots = Vector(
      Slot(InputState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, CancelAction),
        Transition(InputEventGuard, ValidationAction),
        Transition(OkEventGuard, EntityUpdateAction),
        Transition(UpdateEventGuard, EntityUpdateAction)
      )),
      Slot(ConfirmState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, InputAction),
        Transition(OkEventGuard, ShowAction)
      )),
      Slot(ShowState, Transitions(
        Transition(AllGuard, EndAction)
      )),
      Slot(EndState, Transitions(
        Transition(AllGuard, ReturnAction)
      ))
    )
  }

  override def launch(p: Parcel, cmd: ScenarioCandidateCommand): Option[Scenario] = {
    val parcel = p.withUsageKind(CreateUsage)
    def entitytype = cmd.entityType
    def resolveschema(p: Schema): Schema =
      parcel.render.map(_.resolveSchema(entitytype, p)).getOrElse(p)
    if (cmd.name == name) {
      def data = cmd.formRecord
      parcel.context.flatMap(_.
        getEntitySchema(cmd.entityName).map(x => _init(entitytype, resolveschema(x), data)))
    } else {
      None
    }
  }

  private def _init(entity: DomainEntityType, schema: Schema, data: IRecord): UpdateEntityScenario =
    UpdateEntityScenario(InitState, entity, schema, data)

  protected[scenario] def start(p: Parcel, entity: DomainEntityType, schema: Schema, data: IRecord): Parcel = {
    val parcel = p.withUsageKind(CreateUsage)
    val scenario = UpdateEntityScenario(InputState, entity, schema, data)
    InputAction.model(parcel, scenario, schema, data)
  }

  def unmarshallOption(p: String): Option[Scenario] =
    if (p.startsWith("{"))
      _unmarshall_option(p)
    else
      None

  private def _unmarshall_option(p: String): Option[Scenario] = {
    val json = Json.parse(p)
    (json \ "name").asOpt[String].map { name =>
      val state = State.unmarshall((json \ "state").as[String])
      val entity = DomainEntityType((json \ "entity").as[String])
      val schema = Schema.json.unmarshall(json \ "schema")
      val data = Record.create(json \ "data")
      UpdateEntityScenario(state, entity, schema, data)
    }
  }
}

case class DeleteEntityScenario(
  state: State,
  entityType: DomainEntityType,
  override val schema: Schema,
  data: IRecord
) extends Scenario {
  val scenarioClass = DeleteEntityScenario
  override def getSchema = Some(schema)
  val stateMachine = DeleteEntityScenario.stateMachine
  def withState(p: State) = copy(state = p)
  override def start(parcel: Parcel): Parcel = DeleteEntityScenario.start(parcel, entityType, schema, data)

  override protected def adjust_Intent(p: Intent): Intent =
    p.withDomainEntityType(entityType)

  def marshall = Record.data(
    "name" -> DeleteEntityScenario.name,
    "state" -> state.marshall,
    "entity" -> entityType.v,
    "schema" -> schema.toMarshalizable.marshallRecord,
    "data" -> data
  ).toJsonString
}
object DeleteEntityScenario extends ScenarioClass {
  val stateMachine = new StateMachine {
    import StateMachine.Slot
    val slots = Vector(
      Slot(InputState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, CancelAction),
        Transition(InputEventGuard, ValidationAction),
        Transition(OkEventGuard, EntityCreateAction),
        Transition(CreateEventGuard, EntityCreateAction)
      )),
      Slot(ConfirmState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, InputAction),
        Transition(OkEventGuard, ShowAction)
      )),
      Slot(ShowState, Transitions(
        Transition(AllGuard, EndAction)
      )),
      Slot(EndState, Transitions(
        Transition(AllGuard, ReturnAction)
      ))
    )
  }

  override def launch(p: Parcel, cmd: ScenarioCandidateCommand): Option[Scenario] = {
    val parcel = p.withUsageKind(CreateUsage)
    def entitytype = cmd.entityType
    def resolveschema(p: Schema): Schema =
      parcel.render.map(_.resolveSchema(entitytype, p)).getOrElse(p)
    if (cmd.name == name) {
      def data = cmd.formRecord
      parcel.context.flatMap(_.
        getEntitySchema(cmd.entityName).map(x => _init(entitytype, resolveschema(x), data)))
    } else {
      None
    }
  }

  private def _init(entity: DomainEntityType, schema: Schema, data: IRecord): DeleteEntityScenario =
    DeleteEntityScenario(InitState, entity, schema, data)

  protected[scenario] def start(p: Parcel, entity: DomainEntityType, schema: Schema, data: IRecord): Parcel = {
    val parcel = p.withUsageKind(CreateUsage)
    val scenario = DeleteEntityScenario(InputState, entity, schema, data)
    InputAction.model(parcel, scenario, schema, data)
  }

  def unmarshallOption(p: String): Option[Scenario] =
    if (p.startsWith("{"))
      _unmarshall_option(p)
    else
      None

  private def _unmarshall_option(p: String): Option[Scenario] = {
    val json = Json.parse(p)
    (json \ "name").asOpt[String].map { name =>
      val state = State.unmarshall((json \ "state").as[String])
      val entity = DomainEntityType((json \ "entity").as[String])
      val schema = Schema.json.unmarshall(json \ "schema")
      val data = Record.create(json \ "data")
      DeleteEntityScenario(state, entity, schema, data)
    }
  }
}

case class InvokeOperationScenario(
  state: State,
  override val schema: Schema,
  data: IRecord,
  operation: URI,
  operationMethod: Method,
  successView: Option[String],
  errorView: Option[String]
) extends Scenario {
  val scenarioClass = InvokeOperationScenario

  val stateMachine = new StateMachine {
    import StateMachine.Slot
    val slots = Vector(
      Slot(InputState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, CancelAction),
        Transition(InputEventGuard, ValidationAction),
        Transition(OkEventGuard, ExecuteAction(execute)),
        Transition(ExecuteEventGuard, ExecuteAction(execute))
      )),
      Slot(ConfirmState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, InputAction),
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

  def withState(p: State) = copy(state = p)

  override def getSchema = Some(schema)
  override protected def adjust_Intent(p: Intent): Intent = p

  override def getCancelContent: Option[Content] = Some(RedirectContent(""))

  def execute(p: Intent): Intent = {
    p.context.map { ctx =>
      val pathname = "2.1/PalShopApp" // XXX
      val d = data + p.event.getData.getOrElse(Record.empty)
      val (query, form) = operationMethod match {
        case Get => (d, Record.empty)
        case _ => (Record.empty, d)
      }
      val req = Request(pathname, operation.toString, operationMethod.name, query, form)
      val cmd = InvokeOperationCommand(ctx.platformExecutionContext, req)
      val res = ctx.invoke(cmd)
      _response(p, res)
    }.getOrElse(p.goError("Missing scenario execution context"))
  }

  private def _response(p: Intent, res: Response): Intent = {
    val model = res.toModel
    model match {
      case m: ErrorModel =>
        val view = errorView getOrElse("")
        p.error(view, model)
      case m =>
        val view = successView getOrElse("")
        p.success(view, model)
    }
  }

  def marshall = Record.data(
    "name" -> InvokeOperationScenario.name,
    "state" -> state.marshall,
    "data" -> data
  ).toJsonString
}
object InvokeOperationScenario extends ScenarioClass {
  // def launch(p: Parcel, cmd: ScenarioCommand): Option[Scenario] = {
  //   val parcel = p.withUsageKind(CreateUsage)
  //   def entitytype = cmd.entityType
  //   def resolveschema(p: Schema): Schema =
  //     parcel.render.map(_.resolveSchema(entitytype, p)).getOrElse(p)
  //   if (cmd.name == name) {
  //     def data = cmd.formRecord
  //     parcel.context.flatMap(_.
  //       getEntitySchema(cmd.entityName).map(x => _init(entitytype, resolveschema(x), data)))
  //   } else {
  //     None
  //   }
  // }

  // private def _init(entity: DomainEntityType, schema: Schema, data: IRecord): InvokeOperationScenario =
  //   InvokeOperationScenario(InitState, entity, schema, data)

  def launch(p: Parcel, action: InvokeOperationScenarioAction): Parcel = {
    implicit val strategy = p.toStrategy
    val data = p.inputQueryFormParameters
    data.getString(PROP_SCENARIO).flatMap(unmarshallOption(strategy, action, _)).
      map(_go(p, _, data)).
      getOrElse(start(p, action, action.parameters.toSchema, data))
  }

  def start(
    p: Parcel,
    action: InvokeOperationScenarioAction,
    schema: Schema,
    data: IRecord
  ): Parcel = {
    val pathname = _pathname(p)
    val parcel = p.withUsageKind(InvokeUsage) // ???
    val scenario = InvokeOperationScenario(
      InputState,
      schema,
      data,
      action.operation,
      action.effectiveMethod,
      action.successView,
      action.errorView
    )
    val cmd = ScenarioCommand(scenario, pathname, StartEvent(p, data))
    parcel.withCommand(cmd)
  }

  private def _go(
    p: Parcel,
    scenario: InvokeOperationScenario,
    data: IRecord
  ): Parcel = data.getString(PROP_SUBMIT).map(x =>
    Event.get(p, x).
      map { event =>
        val pathname = _pathname(p)
        val cmd = ScenarioCommand(scenario, pathname, event)
        p.withCommand(cmd)
      }.getOrElse(RAISE.notImplementedYetDefect) // TODO WebScenarioDefect)
  ).getOrElse(
    RAISE.notImplementedYetDefect // TODO WebScenarioDefect
  )

  private def _pathname(p: Parcel): PathName = p.command.collect {
    case MaterialCommand(pn) => pn
    case m: IndexCommand => m.pathname
  }.getOrElse(RAISE.noReachDefect)

  def unmarshallOption(p: String): Option[InvokeOperationScenario] = RAISE.unsupportedOperationFault

  def unmarshallOption(
    strategy: RenderStrategy,
    action: InvokeOperationScenarioAction,
    p: String
  ): Option[InvokeOperationScenario] =
    if (p.startsWith("{"))
      _unmarshall_option(strategy, action, p)
    else
      None

  private def _unmarshall_option(
    strategy: RenderStrategy,
    action: InvokeOperationScenarioAction,
    p: String
  ): Option[InvokeOperationScenario] = {
    val json = Json.parse(p)
    (json \ "name").asOpt[String].map { name =>
      val state = State.unmarshall((json \ "state").as[String])
      val data = Record.create(json \ "data")
      InvokeOperationScenario(
        state,
        action.parameters.toSchema(strategy),
        data,
        action.operation,
        action.effectiveMethod,
        action.successView,
        action.errorView
      )
    }
  }

  // def unmarshallOption(p: String): Option[Scenario] =
  //   if (p.startsWith("{"))
  //     _unmarshall_option(p)
  //   else
  //     None

  // private def _unmarshall_option(p: String): Option[Scenario] = {
  //   val json = Json.parse(p)
  //   (json \ "name").asOpt[String].map { name =>
  //     val state = State.unmarshall((json \ "state").as[String])
  //     val entity = DomainEntityType((json \ "entity").as[String])
  //     val schema = Schema.json.unmarshall(json \ "schema")
  //     val data = Record.create(json \ "data")
  //     InvokeOperationScenario(state, entity, schema, data)
  //   }
  // }
}

case class ExecuteScriptScenario(
  state: State,
  override val schema: Schema,
  data: IRecord,
  script: String,
  operationMethod: Method,
  successView: Option[String],
  errorView: Option[String]
) extends Scenario {
  val scenarioClass = ExecuteScriptScenario

  val stateMachine = new StateMachine {
    import StateMachine.Slot
    val slots = Vector(
      Slot(InputState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, CancelAction),
        Transition(InputEventGuard, ValidationAction),
        Transition(OkEventGuard, ExecuteAction(execute)),
        Transition(ExecuteEventGuard, ExecuteAction(execute))
      )),
      Slot(ConfirmState, Transitions(
        Transition(CancelEventGuard, CancelAction),
        Transition(BackEventGuard, InputAction),
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

  def withState(p: State) = copy(state = p)

  override def getSchema = Some(schema)
  override protected def adjust_Intent(p: Intent): Intent = p

  override def getCancelContent: Option[Content] = Some(RedirectContent(""))

  def execute(p: Intent): Intent = {
    p.context.map { ctx =>
      val pathname = "2.1/PalShopApp" // XXX
      val d = data + p.event.getData.getOrElse(Record.empty)
      val (query, form) = operationMethod match {
        case Get => (d, Record.empty)
        case _ => (Record.empty, d)
      }
      val req = Request(pathname, "eval", operationMethod.name, query, form)
      val cmd = ExecuteScriptCommand(
        ctx.platformExecutionContext,
        p.scenario.schema,
        script,
        req
      )
      val res = ctx.execute(cmd)
      _response(p, res)
    }.getOrElse(p.goError("Missing scenario execution context"))
  }

  private def _response(p: Intent, res: Response): Intent = {
    val model = res.toModel
    model match {
      case m: ErrorModel =>
        val view = errorView getOrElse("")
        p.error(view, model)
      case m =>
        val view = successView getOrElse("")
        p.success(view, model)
    }
  }

  def marshall = Record.data(
    "name" -> ExecuteScriptScenario.name,
    "state" -> state.marshall,
    "data" -> data
  ).toJsonString
}
object ExecuteScriptScenario extends ScenarioClass {
  def launch(p: Parcel, action: ExecuteScriptScenarioAction): Parcel = {
    implicit val strategy = p.toStrategy
    val data = p.inputQueryFormParameters
    data.getString(PROP_SCENARIO).flatMap(unmarshallOption(strategy, action, _)).
      map(_go(p, _, data)).
      getOrElse(start(p, action, action.parameters.toSchema, data))
  }

  def start(
    p: Parcel,
    action: ExecuteScriptScenarioAction,
    schema: Schema,
    data: IRecord
  ): Parcel = {
    val pathname = _pathname(p)
    val parcel = p.withUsageKind(InvokeUsage) // ???
    val scenario = ExecuteScriptScenario(
      InputState,
      schema,
      data,
      action.script,
      action.effectiveMethod,
      action.successView,
      action.errorView
    )
    val cmd = ScenarioCommand(scenario, pathname, StartEvent(p, data))
    parcel.withCommand(cmd)
  }

  private def _go(
    p: Parcel,
    scenario: ExecuteScriptScenario,
    data: IRecord
  ): Parcel = data.getString(PROP_SUBMIT).map(x =>
    Event.get(p, x).
      map { event =>
        val pathname = _pathname(p)
        val cmd = ScenarioCommand(scenario, pathname, event)
        p.withCommand(cmd)
      }.getOrElse(RAISE.notImplementedYetDefect) // TODO WebScenarioDefect)
  ).getOrElse(
    RAISE.notImplementedYetDefect // TODO WebScenarioDefect
  )

  private def _pathname(p: Parcel): PathName = p.command.collect {
    case MaterialCommand(pn) => pn
    case m: IndexCommand => m.pathname
  }.getOrElse(RAISE.noReachDefect)

  def unmarshallOption(p: String): Option[ExecuteScriptScenario] = RAISE.unsupportedOperationFault

  def unmarshallOption(
    strategy: RenderStrategy,
    action: ExecuteScriptScenarioAction,
    p: String
  ): Option[ExecuteScriptScenario] =
    if (p.startsWith("{"))
      _unmarshall_option(strategy, action, p)
    else
      None

  private def _unmarshall_option(
    strategy: RenderStrategy,
    action: ExecuteScriptScenarioAction,
    p: String
  ): Option[ExecuteScriptScenario] = {
    val json = Json.parse(p)
    (json \ "name").asOpt[String].map { name =>
      val state = State.unmarshall((json \ "state").as[String])
      val data = Record.create(json \ "data")
      ExecuteScriptScenario(
        state,
        action.parameters.toSchema(strategy),
        data,
        action.script,
        action.effectiveMethod,
        action.successView,
        action.errorView
      )
    }
  }
}

case class LoginScenario(
  state: State,
  data: IRecord,
  usernameLabel: Option[String],
  passwordLabel: Option[String],
  successRedirect: Option[String],
  successView: Option[String],
  errorView: Option[String]
) extends Scenario {
  import LoginScenario._

  val scenarioClass = LoginScenario

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
    import SchemaBuilder._
    SchemaBuilder.create(
      CL(PROP_USERNAME, usernameLabel getOrElse "User"),
      CL(PROP_PASSWORD, passwordLabel getOrElse "Password")
    )
  }

  protected lazy val default_input_form = PropertyInputFormModel(
    new URI(""),
    Post,
    schema,
    data,
    Hiddens.empty,
    Submits(Submit(ExecuteSubmitKind, I18NString("Login")))
  )

  def inputForm(p: Intent): FormModel = default_input_form

  def withState(p: State) = copy(state = p)
  def withData(p: IRecord) = copy(data = p)

  override protected def adjust_Intent(p: Intent): Intent = p

  def execute(p: Intent): Intent = {
    p.context.map { ctx =>
      (data.getString(PROP_USERNAME), data.getString(PROP_PASSWORD)) match {
        case (Some(username), Some(password)) =>
          ctx.login(username, password) match {
            case Right(session) => _response(p, session)
            case Left(c) => _error(p, c)
          }
        case (Some(username), None) => _error(p, Conclusion.missing(PROP_PASSWORD))
        case (None, Some(password)) => _error(p, Conclusion.missing(PROP_USERNAME))
        case (None, None) => _error(p, Conclusion.missing(PROP_USERNAME, PROP_PASSWORD))
      }
    }.getOrElse(_go_error(p, "Missing scenario execution context for login"))
  }

  private def _response(p: Intent, session: Session): Intent = {
    val a = p.withSession(session)
    successRedirect.map(a.setRedirect) orElse
    successView.map(a.withViewCommand) getOrElse a.setRedirect("index")
  }

  private def _error(p: Intent, c: Conclusion): Intent = {
    val model = inputForm(p).setError(c)
    p.withModel(model)
  }

  private def _go_error(p: Intent, msg: String): Intent = {
    val a = p.goError(msg)
    errorView.map(a.withViewCommand) getOrElse a
  }

  def marshall = Record.data(
    "name" -> LoginScenario.name,
    "state" -> state.marshall,
    "data" -> data
  ).toJsonString
}
object LoginScenario extends ScenarioClass {
  val PROP_USERNAME = "username"
  val PROP_PASSWORD = "password"

  def launch(p: Parcel, action: LoginScenarioAction): Parcel = {
    implicit val strategy = p.toStrategy
    val data = p.inputQueryFormParameters
    data.getString(PROP_SCENARIO).flatMap(unmarshallOption(strategy, action, _)).
      map(_go(p, _, data)).
      getOrElse(start(p, action, data))
  }

  def start(
    p: Parcel,
    action: LoginScenarioAction,
    data: IRecord
  ): Parcel = {
    val locale = p.locale
    val pathname = _pathname(p)
    val parcel = p.withUsageKind(InvokeUsage) // ???
    val scenario = LoginScenario(
      InputState,
      data,
      action.usernameLabel.map(_.toString(locale)),
      action.passwordLabel.map(_.toString(locale)),
      action.successRedirect,
      action.successView,
      action.errorView
    )
    val cmd = ScenarioCommand(scenario, pathname, StartEvent(p, data))
    parcel.withCommand(cmd)
  }

  private def _go(
    p: Parcel,
    scenario: LoginScenario,
    data: IRecord
  ): Parcel = data.getString(PROP_SUBMIT).map(x =>
    Event.get(p, x).
      map { event =>
        val pathname = _pathname(p)
        val s = scenario.withData(data)
        val cmd = ScenarioCommand(s, pathname, event)
        p.withCommand(cmd)
      }.getOrElse(RAISE.notImplementedYetDefect) // TODO WebScenarioDefect)
  ).getOrElse(
    RAISE.notImplementedYetDefect // TODO WebScenarioDefect
  )

  private def _pathname(p: Parcel): PathName = p.command.collect {
    case MaterialCommand(pn) => pn
  }.getOrElse(RAISE.noReachDefect)

  def unmarshallOption(p: String): Option[LoginScenario] = RAISE.unsupportedOperationFault

  def unmarshallOption(
    strategy: RenderStrategy,
    action: LoginScenarioAction,
    p: String
  ): Option[LoginScenario] =
    if (p.startsWith("{"))
      _unmarshall_option(strategy, action, p)
    else
      None

  private def _unmarshall_option(
    strategy: RenderStrategy,
    action: LoginScenarioAction,
    p: String
  ): Option[LoginScenario] = {
    val locale = strategy.locale
    val json = Json.parse(p)
    (json \ "name").asOpt[String].map { name =>
      val state = State.unmarshall((json \ "state").as[String])
      val data = Record.create(json \ "data")
      LoginScenario(
        state,
        data,
        action.usernameLabel.map(_.toString(locale)),
        action.passwordLabel.map(_.toString(locale)),
        action.successRedirect,
        action.successView,
        action.errorView
      )
    }
  }
}

/*
 * State
 */
trait StateClass {
  def name: String = StringUtils.classNameToHypenName("State", this)
  def isAccept(p: State): Boolean = p.name == name
  def unmarshall(p: String): State
  def unmarshallOption(p: String): Option[State] =
    if (p == name)
      Some(unmarshall(p))
    else
      None
}

trait State {
  def stateClass: StateClass
  def name: String = stateClass.name
//  def method: Method = Post
//  def apply(evt: Intent): Intent
  def marshall: String = name
}
object State {
  // val states = Vector(input, confirm, show, end)
  val states = Vector(
    InitState,
    InputState,
    ConfirmState,
    ShowState,
    EndState,
    FinalState
  )

  def unmarshall(p: String): State = unmarshallOption(p) getOrElse {
    RAISE.notImplementedYetDefect
  }
  def unmarshallOption(p: String): Option[State] = states.toStream.flatMap(_.unmarshallOption(p)).headOption
}

case object InitState extends State with StateClass {
  override val name = super.name
  val stateClass = this
  def unmarshall(p: String): State = this
}
case object InputState extends State with StateClass {
  override val name = super.name
  val stateClass = this
  def unmarshall(p: String): State = this
}
case object ConfirmState extends State with StateClass {
  override val name = super.name
  val stateClass = this
  def unmarshall(p: String): State = this
}
case object ShowState extends State with StateClass {
  override val name = super.name
  val stateClass = this
  def unmarshall(p: String): State = this
}
case object SuccessState extends State with StateClass {
  override val name = super.name
  val stateClass = this
  def unmarshall(p: String): State = this
}
case object ErrorState extends State with StateClass {
  override val name = super.name
  val stateClass = this
  def unmarshall(p: String): State = this
}
case object EndState extends State with StateClass {
  override val name = super.name
  val stateClass = this
  def unmarshall(p: String): State = this
}
case object FinalState extends State with StateClass {
  override val name = super.name
  val stateClass = this
  def unmarshall(p: String): State = this
}

trait StateMachine {
  def slots: Vector[StateMachine.Slot]
  private lazy val _slots = slots.toStream
  def apply(intent: Intent): Intent = {
    val r = _slots.flatMap(_.applyOption(intent)).headOption orElse {
      apply_Intent(intent)
    } getOrElse {
      intent.goUnknownEvent(intent.event)
    }
    r.state match {
      case EndState => apply(r.withEvent(EndEvent(r.parcel)))
      case FinalState => r
      case _ => r
    }
  }

  protected def apply_Intent(intent: Intent): Option[Intent] = None
}
object StateMachine {
  case class Slot(
    stateClass: StateClass,
    transitions: Transitions
  ) {
    def applyOption(intent: Intent): Option[Intent] =
      if (stateClass.isAccept(intent.state))
        transitions.applyOption(intent)
      else
        None
  }
}

/*
 * Event
 */
trait EventClass {
  def isAccept(p: Event): Boolean
}

trait Event {
  def parcel: Parcel
//  def withModel(p: Model): Event
  def withParcel(p: Parcel): Event
  def getData: Option[IRecord]
}
object Event {
  val EVENT_INPUT = "input"
  val EVENT_OK = "ok" // execute, or confirm if required
  val EVENT_EXECUTE = "execute" // force execute
  val EVENT_CANCEL = "cancel"
  val EVENT_CREATE = "create"
  val EVENT_UPDATE = "update"
  val EVENT_DELETE = "delete"
  val EVENT_BACK = "back"
  val EVENT_SEARCH = "search"
  val EVENT_EXCEPTION = "exception"
  val EVENT_END = "end"

  def get(parcel: Parcel, cmd: ScenarioCandidateCommand): Option[Event] = {
    val name: String = cmd.getSubmit getOrElse {
      RAISE.noReachDefect
    }
    def data = cmd.formRecord
    def e = cmd.exception getOrElse {
      RAISE.noReachDefect
    }
    _get(parcel, name, data, e)
  }

  def get(parcel: Parcel, name: String): Option[Event] = {
    val data = parcel.inputQueryFormParameters
    _get(parcel, name, data)
  }

  private def _get(
    parcel: Parcel,
    name: String,
    data: => IRecord,
    e: => Throwable = RAISE.noReachDefect
  ): Option[Event] =
    name match {
      case EVENT_INPUT => Some(InputEvent(parcel, data))
      case EVENT_OK => Some(OkEvent(parcel, data))
      case EVENT_EXECUTE => Some(ExecuteEvent(parcel))
      case EVENT_CANCEL => Some(CancelEvent(parcel))
      case EVENT_CREATE => Some(CreateEvent(parcel))
      case EVENT_UPDATE => Some(UpdateEvent(parcel))
      case EVENT_DELETE => Some(DeleteEvent(parcel))
      case EVENT_BACK => Some(BackEvent(parcel))
      case EVENT_EXCEPTION => Some(ExceptionEvent(parcel, e))
      case EVENT_END => Some(EndEvent(parcel))
      case _ => None
    }
}

case class StartEvent(parcel: Parcel, data: IRecord) extends Event {
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = Some(data)
}
case class InputEvent(parcel: Parcel, data: IRecord) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = Some(data)
}
case class OkEvent(parcel: Parcel, data: IRecord) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = Some(data)
}
case class ExecuteEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = None
}
case class CancelEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = None
}
case class CreateEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = None
}
case class UpdateEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = None
}
case class DeleteEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = None
}
case class BackEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = None
}
case class ExceptionEvent(parcel: Parcel, e: Throwable) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = None
}
case class EndEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
  def getData: Option[IRecord] = None
}

/*
 * Intent
 */
case class Intent(
  event: Event,
  scenario: Scenario,
  state: State,
  parcel: Parcel,
  getDomainEntityType: Option[DomainEntityType], // XXX migrate to scenario
  getDomainEntityId: Option[DomainObjectId] // XXX migrate to scenario
) {
  def withEvent(p: Event) = copy(event = p)
  def withState(p: State) = copy(state = p)
  def withCommand(p: Command) = copy(parcel = parcel.withCommand(p))
  def withModel(p: Model): Intent = copy(parcel = parcel.withModel(p))
  def success(view: String, model: Model): Intent = copy(
    state = SuccessState,
    parcel = parcel.withViewModel(view, model)
  )
  def error(view: String, model: Model): Intent = copy(
    state = ErrorState,
    parcel = parcel.withViewModel(view, model)
  )
  def toEvent = (scenario.withState(state), event.withParcel(parcel))
  def context: Option[ExecutionContext] = parcel.context
  def inputFormParameters: IRecord = parcel.inputFormParameters
  def controllerUri: URI = parcel.controllerUri
  def domainEntityType: DomainEntityType = getDomainEntityType getOrElse {
    RAISE.noReachDefect
  }
  def domainEntityId: DomainObjectId = getDomainEntityId getOrElse {
    RAISE.noReachDefect
  }
  def executionContext: ExecutionContext = parcel.context getOrElse {
    RAISE.noReachDefect
  }

  def toStrategy = parcel.toStrategy

  def goOrigin: Intent = copy(parcel = parcel.goOrigin)
  def goError(p: Throwable): Intent = copy(parcel = parcel.goError(p))
  def goError(p: Invalid): Intent = copy(parcel = parcel.goError(p))
  def goError(msg: String): Intent = copy(parcel = parcel.goError(msg))
  def goUnknownEvent(p: Event): Intent = copy(parcel = parcel.goError("Unknown scenario event: $p"))
  def withDomainEntityType(p: DomainEntityType) = copy(getDomainEntityType = Some(p))
  def withDomainEntityId(p: DomainObjectId) = copy(getDomainEntityId = Some(p))
  def goCancel = scenario.getCancelContent match {
    case Some(s) => copy(parcel = parcel.withContent(s))
    case None => withReturn
  }
  // legacy
  def withReturn = scenario.getCallerUri.fold(
    copy(parcel = parcel.withContent(RedirectContent("../../index.html")))
  )(x =>
    copy(parcel = parcel.withContent(RedirectContent(s"../../$x"))))
  def withSession(p: Session): Intent = copy(parcel = parcel.withSession(p))
  def withViewCommand(p: String): Intent = copy(parcel = parcel.withCommand(ViewCommand(p)))
  def setRedirect(p: String): Intent = copy(parcel = parcel.setRedirect(p))
}
object Intent {
  def apply(
    event: Event,
    scenario: Scenario,
    state: State
  ): Intent = Intent(
    event, scenario, state, event.parcel, None, None
  )
}

/*
 * Action
 */
trait Action {
  def apply(p: Intent): Intent

  protected def hidden_scenario(scenario: Scenario, state: State): Hiddens =
    // Hidden(Some(scenario.withState(state).marshall))
    Hiddens.scenario(scenario.withState(state).marshall)

  protected def button_input(strategy: RenderStrategy) = 
    Submit(InputSubmitKind, strategy.applicationRule.submitLabel(InputSubmitKind))

  protected def button_cancel(strategy: RenderStrategy) = 
    Submit(CancelSubmitKind, strategy.applicationRule.submitLabel(CancelSubmitKind))
}

trait SchemaActionBase extends Action {
}

case object CancelAction extends Action {
  def apply(p: Intent): Intent = p.goCancel // withReturn
}

case object ValidationAction extends SchemaActionBase {
  def apply(p: Intent): Intent = {
    val scenario = p.scenario
    val schema = scenario.schema
    val data = p.inputFormParameters
    // TODO validate
//    val uri = p.controllerUri
    val uri = new URI("")
    val state = ConfirmState
    val m = model(scenario, schema, data, uri, Post, state)
    p.withModel(m).withState(state)
  }

  def model(scenario: Scenario, schema: Schema, data: IRecord, uri: URI, method: Method, state: State): Model = {
    val submits = Submits(Vector(
      Submit(OkSubmitKind),
      Submit(BackSubmitKind),
      Submit(CancelSubmitKind)
    ))
    val hidden = hidden_scenario(scenario, state)
    PropertyConfirmFormModel(uri, method, schema, data, hidden, submits)
  }
}

case object InputAction extends SchemaActionBase {
  def apply(p: Intent): Intent = {
    val scenario = p.scenario
    val method = scenario.method
    val schema = scenario.schema
    val data = p.inputFormParameters
    model(p, scenario, schema, data)
  }

  def model(p: Parcel, scenario: Scenario, params: Parameters, data: IRecord): Parcel =
    model(p, scenario, params.toSchema(p.toStrategy), data)

  def model(p: Parcel, scenario: Scenario, schema: Schema, data: IRecord): Parcel = {
    val (m, s) = model(p.toStrategy, scenario, schema, data)
    p.withModel(m)
  }

  def model(p: Intent, scenario: Scenario, schema: Schema, data: IRecord): Intent = {
    val (m, s) = model(p.toStrategy, scenario, schema, data)
    p.withModel(m).withState(s)
  }

  def model(strategy: RenderStrategy, scenario: Scenario, schema: Schema, data: IRecord): (Model, State) = {
    model(strategy, scenario, schema, data, new URI(""), Post)
  }

  def model(strategy: RenderStrategy, scenario: Scenario, schema: Schema, data: IRecord, formaction: URI, method: Method): (Model, State) = {
    val input = button_input(strategy)
    val cancel = button_cancel(strategy)
    val state = InputState
    val submits = Submits(input, cancel)
    val hidden = hidden_scenario(scenario, state)
    val m = PropertyInputFormModel(
      formaction,
      method,
      schema,
      data,
      hidden,
      submits
    )
    (m, state)
  }
}

case object ShowAction extends Action {
  def apply(p: Intent): Intent = {
    val scenario = p.scenario
    val method = scenario.method
    val schema = scenario.schema
    val data = p.inputFormParameters
//    val uri = p.controllerUri
    val uri = new URI("")
    val m = model(scenario, schema, data, uri, method)
    p.withModel(m)
  }

  def model(scenario: Scenario, schema: Schema, data: IRecord, uri: URI, method: Method): Model = {
    val state = ShowState
    val submits = Submits(Vector(
      Submit(OkSubmitKind)
    ))
    val hidden = hidden_scenario(scenario, state)
    PropertyConfirmFormModel(uri, method, schema, data, hidden, submits)
  }
}

case object ReturnAction extends Action {
  def apply(p: Intent): Intent = {
    p.withReturn.withState(FinalState)
  }
}

case object EndAction extends Action {
  def apply(p: Intent): Intent = {
    p.withReturn.withState(FinalState)
  }
}

case object EntityCreateAction extends Action {
  def apply(p: Intent): Intent = {
    val scenario = p.scenario
    val rsc = p.domainEntityType
    val data = p.inputFormParameters
    val ctx = p.executionContext
    val id = ctx.createEntity(rsc, data)
    p.withState(EndState)
  }
}

case object EntityUpdateAction extends Action {
  def apply(p: Intent): Intent = {
    val scenario = p.scenario
    val rsc = p.domainEntityType
    val id = p.domainEntityId
    val data = p.inputFormParameters
    val ctx = p.executionContext
    ctx.updateEntity(rsc, id, data)
    p.withState(EndState)
  }
}

case class ExecuteAction(f: Intent => Intent) extends Action {
  def apply(p: Intent): Intent = f(p)
}

// case object InvokeOperationAction extends Action {
//   def apply(p: Intent): Intent = {
//     p.context.map { ctx =>
//       val req = ???
//       val cmd = InvokeOperationCommand(req)
//       val res = ctx.invoke(cmd)
//       val model = res.toModel
//       ???
//     }.getOrElse(p.goError("Missing scenario execution context"))
//   }
// }

trait Guard {
  def isAccept(evt: Intent): Boolean
}

case object AllGuard extends Guard {
  def isAccept(p: Intent): Boolean = true
}

case object OkEventGuard extends Guard {
  def isAccept(p: Intent): Boolean = p.event.isInstanceOf[OkEvent]
}

case object ExecuteEventGuard extends Guard {
  def isAccept(p: Intent): Boolean = p.event.isInstanceOf[ExecuteEvent]
}

case object CancelEventGuard extends Guard {
  def isAccept(p: Intent): Boolean = p.event.isInstanceOf[CancelEvent]
}

case object BackEventGuard extends Guard {
  def isAccept(p: Intent): Boolean = p.event.isInstanceOf[BackEvent]
}

case object InputEventGuard extends Guard {
  def isAccept(p: Intent): Boolean = p.event.isInstanceOf[InputEvent]
}

case object CreateEventGuard extends Guard {
  def isAccept(p: Intent): Boolean = p.event.isInstanceOf[CreateEvent]
}

case object UpdateEventGuard extends Guard {
  def isAccept(p: Intent): Boolean = p.event.isInstanceOf[UpdateEvent]
}

case class Transitions(
  transitions: Vector[Transition]
) {
  def applyOption(p: Intent): Option[Intent] =
    transitions.toStream.flatMap(_.applyOption(p)).headOption
}
object Transitions {
  def apply(): Transitions = Transitions(Vector.empty)
  def apply(p: Transition, ps: Transition*): Transitions = Transitions(
    (p +: ps).toVector
  )
}

case class Transition(guard: Guard, actions: Seq[Action]) {
  def applyOption(p: Intent): Option[Intent] =
    if (guard.isAccept(p))
      Some(actions./:(p)((z, x) => x.apply(z)))
    else
      None
}
object Transition {
  def apply(guard: Guard, p: Action, ps: Action*): Transition = Transition(
    guard, (p +: ps).toVector
  )
}
