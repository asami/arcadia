package arcadia.scenario

import java.net.URI
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.value.{NamedValueInstance, EnumerationClass}
import org.goldenport.io.UriUtils
import org.goldenport.util.StringUtils
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => _, Schema}
import org.goldenport.record.v2.util.RecordUtils
import arcadia._
import arcadia.context._
import arcadia.model._
import arcadia.view._
import arcadia.domain._

/*
 * @since   Sep. 16, 2017
 *  version Oct. 25, 2017
 *  version Nov. 16, 2017
 *  version Jan.  8, 2018
 *  version Apr.  8, 2018
 *  version Jul. 23, 2018
 *  version Sep.  1, 2018
 *  version Nov.  7, 2018
 * @version Jul. 29, 2019
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
  def start(parcel: Parcel): Parcel
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

  def launch(parcel: Parcel, cmd: ScenarioCommand): Option[Scenario] =
    scenarios_stream.flatMap(_.launch(parcel, cmd)).headOption

  def unmarshall(p: String): Scenario = unmarshallOption(p) getOrElse {
    RAISE.noReachDefect
  }

  def unmarshallOption(p: String): Option[Scenario] = scenarios.toStream.flatMap(_.unmarshallOption(p)).headOption
}

trait ScenarioClass {
  lazy val _name: String = StringUtils.classNameToHypenName("Scenario", this)
  def name: String = _name
  def launch(parcel: Parcel, cmd: ScenarioCommand): Option[Scenario]
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
  def start(parcel: Parcel): Parcel = CreateEntityScenario.start(parcel, entityType, schema, data)

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

  def launch(p: Parcel, cmd: ScenarioCommand): Option[Scenario] = {
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
  def start(parcel: Parcel): Parcel = UpdateEntityScenario.start(parcel, entityType, schema, data)

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

  def launch(p: Parcel, cmd: ScenarioCommand): Option[Scenario] = {
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
  def start(parcel: Parcel): Parcel = DeleteEntityScenario.start(parcel, entityType, schema, data)

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

  def launch(p: Parcel, cmd: ScenarioCommand): Option[Scenario] = {
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

trait EventClass {
  def isAccept(p: Event): Boolean
}

trait Event {
  def parcel: Parcel
//  def withModel(p: Model): Event
  def withParcel(p: Parcel): Event
}
object Event {
  val EVENT_INPUT = "input"
  val EVENT_OK = "ok"
  val EVENT_CANCEL = "cancel"
  val EVENT_CREATE = "create"
  val EVENT_UPDATE = "update"
  val EVENT_DELETE = "delete"
  val EVENT_BACK = "back"
  val EVENT_SEARCH = "search"
  val EVENT_EXECUTE = "execute"
  val EVENT_EXCEPTION = "exception"
  val EVENT_END = "end"

  def get(parcel: Parcel, cmd: ScenarioCommand): Option[Event] = {
    val name: String = cmd.getSubmit getOrElse {
      RAISE.noReachDefect
    }
    def data = cmd.formRecord
    def e = cmd.exception getOrElse {
      RAISE.noReachDefect
    }
    name match {
      case EVENT_INPUT => Some(InputEvent(parcel, data))
      case EVENT_OK => Some(OkEvent(parcel))
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
}

case class InputEvent(parcel: Parcel, data: IRecord) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
}
case class OkEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
}
case class CancelEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
}
case class CreateEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
}
case class UpdateEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
}
case class DeleteEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
}
case class BackEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
}
case class ExceptionEvent(parcel: Parcel, e: Throwable) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
}
case class EndEvent(parcel: Parcel) extends Event {
//  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
  def withParcel(p: Parcel) = copy(parcel = p)
}

case class Intent(
  event: Event,
  scenario: Scenario,
  state: State,
  parcel: Parcel,
  getDomainEntityType: Option[DomainEntityType],
  getDomainEntityId: Option[DomainObjectId]
) {
  def withEvent(p: Event) = copy(event = p)
  def withState(p: State) = copy(state = p)
  def withModel(p: Model): Intent = copy(parcel = parcel.withModel(p))
  def toEvent = (scenario.withState(state), event.withParcel(parcel))
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
  def goError(msg: String): Intent = copy(parcel = parcel.goError(msg))
  def goUnknownEvent(p: Event): Intent = copy(parcel = parcel.goError("Unknown scenario event: $p"))
  def withDomainEntityType(p: DomainEntityType) = copy(getDomainEntityType = Some(p))
  def withDomainEntityId(p: DomainObjectId) = copy(getDomainEntityId = Some(p))
  def withReturn = scenario.getCallerUri.fold(
    copy(parcel = parcel.withContent(RedirectContent("../../index.html")))
  )(x =>
    copy(parcel = parcel.withContent(RedirectContent(s"../../$x"))))
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

trait Action {
  def apply(p: Intent): Intent

  protected def hidden_scenario(scenario: Scenario, state: State): Hiddens =
    // Hidden(Some(scenario.withState(state).marshall))
    Hiddens.scenario(scenario.withState(state).marshall)

  protected def button_input(strategy: RenderStrategy) = 
    Submit(InputSubmitKind, strategy.application.submitLabel(InputSubmitKind))

  protected def button_cancel(strategy: RenderStrategy) = 
    Submit(CancelSubmitKind, strategy.application.submitLabel(CancelSubmitKind))
}

trait SchemaActionBase extends Action {
}

case object CancelAction extends Action {
  def apply(p: Intent): Intent = p.withReturn
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
//    val uri = p.controllerUri
    // val uri = new URI("")
    // val m = model(scenario, schema, data, uri, method)
    // p.withModel(m)
    model(p, scenario, schema, data)
  }

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

  def model(strategy: RenderStrategy, scenario: Scenario, schema: Schema, data: IRecord, uri: URI, method: Method): (Model, State) = {
    val input = button_input(strategy)
    val cancel = button_cancel(strategy)
    val state = InputState
    val submits = Submits(input, cancel)
    val hidden = hidden_scenario(scenario, state)
    val m = PropertyInputFormModel(uri, method, schema, data, hidden, submits)
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

trait Guard {
  def isAccept(evt: Intent): Boolean
}

case object AllGuard extends Guard {
  def isAccept(p: Intent): Boolean = true
}

case object OkEventGuard extends Guard {
  def isAccept(p: Intent): Boolean = p.event.isInstanceOf[OkEvent]
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
