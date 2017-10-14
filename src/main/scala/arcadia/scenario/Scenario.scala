package arcadia.scenario

import java.net.URI
import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.value.{NamedValueInstance, EnumerationClass}
import org.goldenport.io.UriUtils
import org.goldenport.util.StringUtils
import org.goldenport.record.v2.{Record, Schema}
import org.goldenport.record.v2.util.RecordUtils
import arcadia._
import arcadia.context._
import arcadia.model._
import arcadia.domain._

/*
 * @since   Sep. 16, 2017
 * @version Oct.  7, 2017
 * @author  ASAMI, Tomoharu
 */
trait Scenario {
  def scenarioClass: ScenarioClass
  def scenarioName = scenarioClass.name
  def state: State
  def method: Method = state.method
  def schema: Schema = getSchema getOrElse RAISE.noReachDefect // ScenarioDefect
  def getSchema: Option[Schema] = None
  def start(parcel: Parcel): Parcel
  def execute(evt: Event): Parcel = apply(evt)._2.parcel
  def apply(evt: Event): (Scenario, Event) = {
    val p = StateTransitionParcel(evt, this, state)
    val r = state.apply(p)
    r.toScenarioEvent
  }
  def withState(p: State): Scenario
  def marshall: String
}
object Scenario {
  val scenarios = Vector(CreateEntityScenario)
  val scenarios_stream = scenarios.toStream

  def get(parcel: Parcel, cmd: ScenarioCommand): Option[Scenario] =
    scenarios_stream.flatMap(_.get(parcel, cmd)).headOption

  def unmarshall(p: String): Scenario = unmarshallOption(p) getOrElse {
    RAISE.noReachDefect
  }

  def unmarshallOption(p: String): Option[Scenario] = scenarios.toStream.flatMap(_.unmarshallOption(p)).headOption
}

trait ScenarioClass {
  def name: String = StringUtils.classNameToHypenName("Scenario", this)
  def get(parcel: Parcel, cmd: ScenarioCommand): Option[Scenario]
  def unmarshallOption(p: String): Option[Scenario]
}

case class CreateEntityScenario(
  state: State,
  override val schema: Schema,
  data: Record
) extends Scenario {
  val scenarioClass = CreateEntityScenario
  override def getSchema = Some(schema)
  def withState(p: State) = copy(state = p)
  def start(parcel: Parcel): Parcel = CreateEntityScenario.start(parcel, schema, data)

  // def start(parcel: Parcel): Parcel = {
  //   val submit = Submit() // TODO
  //   val state = ???
  //   val hidden = ???
  //   val model = PropertyFormModel(schema, data, hidden, submit)
  //   parcel.withModel(model)
  // }

  def marshall = Record.dataApp(
      "name" -> CreateEntityScenario.name,
      "state" -> state.marshall,
      "schema" -> schema.marshallRecord,
      "data" -> data
  ).toJsonString
}
object CreateEntityScenario extends ScenarioClass {
//  val STATE_INPUT = "input"

  // case class InitState(
  //   schema: Schema,
  //   data: Record
  // ) extends State {
  //   def apply(evt: Event): Parcel = {
  //     val submit = Submit() // TODO
  //     val hidden = ???
  //     val model = PropertyFormModel(schema, data, hidden, submit)
  //     evt.parcel.withModel(model)
  //     // evt match {
  //     //   case CancelEvent => parcel.goOrigin
  //     //   case m: InputEvent => ???
  //     //   case m: ConfirmingEvent => ???
  //     //   case m: ShowEvent => ???
  //     //   case m: ExecuteEvent => ???
  //     //   case m: ExceptionEvent => parcel.goError(m.e)
  //     //   case m => parcel.goError(m.toString)
  //     // }
  //   }
  // }
  // val InputState = TransitionState(
  //   Transition(
  //     Vector(
  //       Transition.Slot(CancelEventGuard, Vector(CancelAction)),
  //       Transition.Slot(InputEventGuard, Vector(ValidationAction))
  //     )
  //   )
  // )

  // val ConfirmState = TransitionState(
  //   Transition(
  //     Vector(
  //       Transition.Slot(CancelEventGuard, Vector(CancelAction)),
  //       Transition.Slot(OkEventGuard, Vector(ShowAction)),
  //       Transition.Slot(BackEventGuard, Vector(InputAction))
  //     )
  //   )
  // )

  // val ShowState = TransitionState(
  //   Transition(
  //     Vector(
  //     )
  //   )
  // )

  // val EndState = TransitionState(
  //   Transition(
  //     Vector(
  //     )
  //   )
  // )

  // case class InputState(transition: Transition) extends State {
  //   def apply(evt: Event): Parcel = {
  //     evt match {
  //       case m: CancelEvent => evt.parcel.goOrigin
  //       case m: ExceptionEvent => evt.parcel.goError(m.e)
  //       case m: InputEvent => transition.fire(evt) getOrElse
  //         evt.parcel.goUnknownEvent(evt)
  //       case m => evt.parcel.goError(m.toString)
  //     }
  //   }
  // }
  // case class ConfirmState() extends State {
  //   def apply(evt: Event): Parcel = ???
  // }
  // case class ShowState() extends State {
  //   def apply(evt: Event): Parcel = ???
  // }

  // def getState(s: String): Option[State] = get_state({
  //   case STATE_INPUT => InputState
  // })(s)

  // protected def get_state(pf: PartialFunction[String, State])(s: String): Option[State] =
  //   pf.lift(s)

  // def apply(schema: Schema, data: Record): CreateEntityScenario =
  //   CreateEntityScenario(schema, data)

  def get(parcel: Parcel, cmd: ScenarioCommand): Option[Scenario] = {
    if (cmd.name == name) {
      def data = cmd.formRecord
      parcel.context.flatMap(_.
        getEntitySchema(cmd.entityName).map(init(_, data)))
    } else {
      None
    }
  }

  def init(schema: Schema, data: Record): CreateEntityScenario =
    CreateEntityScenario(State.input, schema, data)

  def start(parcel: Parcel, schema: Schema, data: Record): Parcel = {
    val state = State.input
    val scenario = CreateEntityScenario(state, schema, data)
    val uri = UriUtils.addPath(parcel.controllerUri, name)
    val submits = Submits(Vector(
      Submit(OkSubmitKind),
      Submit(CancelSubmitKind)
    ))
    val hidden = Hidden(
      Some(scenario.marshall)
    )
    val m = InputAction.model(scenario, schema, data, uri, Post)
    parcel.withModel(m)
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
      val schema = Schema.json.unmarshall((json \ "schema").as[String])
      val data = RecordUtils.js2record((json \ "data").as[JsObject])
      CreateEntityScenario(state, schema, data)
    }
  }
}

// case class EntityCreateScenario(
//   state: State
// ) extends Scenario {
// }

// case class EntityUpdateScenario(
// ) extends Scenario {
// }

// case class EntityDeleteScenario(
// ) extends Scenario {
// }

// case class NavigationSecenario(
// ) extends Scenario {
// }

trait State {
  def name: String
  def method: Method = Post
  def apply(evt: StateTransitionParcel): StateTransitionParcel
  def marshall: String
  def unmarshallOption(p: String): Option[State]
}
object State {
  val STATE_INPUT = "input"
  val STATE_CONFIRM = "confirm"
  val STATE_SHOW = "show"
  val STATE_END = "end"

  val input = TransitionState(
    STATE_INPUT,
    Transition(
      Vector(
        Transition.Slot(CancelEventGuard, Vector(CancelAction)),
        Transition.Slot(InputEventGuard, Vector(ValidationAction))
      )
    )
  )

  val confirm = TransitionState(
    STATE_CONFIRM,
    Transition(
      Vector(
        Transition.Slot(CancelEventGuard, Vector(CancelAction)),
        Transition.Slot(OkEventGuard, Vector(ShowAction)),
        Transition.Slot(BackEventGuard, Vector(InputAction))
      )
    )
  )

  val show = TransitionState(
    STATE_SHOW,
    Transition(
      Vector(
      )
    )
  )

  val end = TransitionState(
    STATE_END,
    Transition(
      Vector(
      )
    )
  )

  val states = Vector(input, confirm, show, end)

  def unmarshall(p: String): State = unmarshallOption(p) getOrElse {
    RAISE.notImplementedYetDefect
  }
  def unmarshallOption(p: String): Option[State] = states.toStream.flatMap(_.unmarshallOption(p)).headOption
}

trait TransitionStateBase extends State {
  def name: String
  def transition: Transition

  def apply(p: StateTransitionParcel): StateTransitionParcel = {
    p.event match {
      case m: CancelEvent => p // evt.parcel.goOrigin
      case m: ExceptionEvent => p // evt.parcel.goError(m.e)
      case m: InputEvent => transition.applyOption(p) getOrElse
        p // evt.parcel.goUnknownEvent(evt)
      case m => p // evt.parcel.goError(m.toString)
    }
  }

  def marshall: String = name
  def unmarshallOption(p: String) = if (p == name) Some(this) else None
}

case class TransitionState(
  name: String,
  transition: Transition
) extends TransitionStateBase {
}

// trait GuardState extends State {
//   def handler: GuardActionHandler

//   def apply(evt: Event): Parcel = {
//     handler.get(evt).map(_.apply(evt).parcel) getOrElse ???
//   }
// }

trait Event {
  def parcel: Parcel
  def withModel(p: Model): Event
}
object Event {
  val EVENT_INPUT = "input"
  val EVENT_OK = "ok"
  val EVENT_CANCEL = "cancel"
  val EVENT_BACK = "back"
  val EVENT_EXCEPTION = "exception"

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
      case EVENT_BACK => Some(BackEvent(parcel))
      case EVENT_EXCEPTION => Some(ExceptionEvent(parcel, e))
      case _ => None
    }
  }

  // def get(parcel: Parcel): Option[Event] = {
  //   val name: String = parcel.eventName
  //   def data = parcel.inputFormParameters
  //   def e = parcel.exception
  //   name match {
  //     case EVENT_INPUT => Some(InputEvent(parcel, data))
  //     case EVENT_OK => Some(OkEvent(parcel))
  //     case EVENT_CANCEL => Some(CancelEvent(parcel))
  //     case EVENT_BACK => Some(BackEvent(parcel))
  //     case EVENT_EXCEPTION => Some(ExceptionEvent(parcel, e))
  //     case _ => None
  //   }
  // }
}

case class InputEvent(parcel: Parcel, data: Record) extends Event {
  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
}
case class OkEvent(parcel: Parcel) extends Event {
  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
}
case class CancelEvent(parcel: Parcel) extends Event {
  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
}
case class BackEvent(parcel: Parcel) extends Event {
  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
}
case class ExceptionEvent(parcel: Parcel, e: Throwable) extends Event {
  def withModel(p: Model): Event = copy(parcel = parcel.withModel(p))
}

case class StateTransitionParcel(event: Event, scenario: Scenario, state: State) {
  def withState(p: State) = copy(state = p)
  def withModel(p: Model): StateTransitionParcel = copy(event = event.withModel(p))
  def toScenarioEvent = (scenario.withState(state), event)
  def inputFormParameters: Record = event.parcel.inputFormParameters
  def controllerUri: URI = event.parcel.controllerUri
  def domainEntityType: DomainEntityType = event.parcel.domainEntityType
  def domainEntityId: DomainObjectId = event.parcel.domainEntityId
  def executionContext: ExecutionContext = event.parcel.context getOrElse {
    RAISE.noReachDefect
  }
}

// case class ConfirmingEvent(data: Record) extends Event {
// }

// case class ShowEvent(data: Record) extends Event {
// }

// case class ExecuteEvent(data: Record) extends Event {
// }

// case class StateEventTuple(state: State, event: Event) {
//   def tuple: (State, Event) = (state, event)
// }

trait Action {
  def apply(p: StateTransitionParcel): StateTransitionParcel
}

trait SchemaActionBase extends Action {
  def apply(p: StateTransitionParcel): StateTransitionParcel = {
    val scenario = p.scenario
    val method = scenario.method
    val schema = scenario.schema
    val data = p.inputFormParameters
    // TODO validate
    val uri = p.controllerUri
    val m = InputAction.model(scenario, schema, data, uri, method)
    p.withModel(m).withState(State.confirm)
  }
}

case object CancelAction extends Action {
  def apply(p: StateTransitionParcel): StateTransitionParcel = p
}

case object ValidationAction extends Action {
  def apply(p: StateTransitionParcel): StateTransitionParcel = {
    val scenario = p.scenario
    val schema = scenario.schema
    val data = p.inputFormParameters
    // TODO validate
    val uri = p.controllerUri
    val m = InputAction.model(scenario, schema, data, uri, Post)
    p.withModel(m).withState(State.confirm)
  }
}

case object InputAction extends Action {
  def apply(p: StateTransitionParcel): StateTransitionParcel = {
    val scenario = p.scenario
    val method = scenario.method
    val schema = scenario.schema
    val data = p.inputFormParameters
    val uri = p.controllerUri
    val m = model(scenario, schema, data, uri, method)
    p.withModel(m)
  }

  def model(scenario: Scenario, schema: Schema, data: Record, uri: URI, method: Method): Model = {
    val state = State.input
    val submits = Submits(Vector(
      Submit(OkSubmitKind),
      Submit(CancelSubmitKind)
    ))
    val hidden = Hidden(
      Some(scenario.marshall)
    )
    PropertyFormModel(uri, method, schema, data, hidden, submits)
  }
}

case object ShowAction extends Action {
  def apply(p: StateTransitionParcel): StateTransitionParcel = {
    val scenario = p.scenario
    val method = scenario.method
    val schema = scenario.schema
    val data = p.inputFormParameters
    val uri = p.controllerUri
    val m = model(scenario, schema, data, uri, method)
    p.withModel(m)
  }

  def model(scenario: Scenario, schema: Schema, data: Record, uri: URI, method: Method): Model = {
    val state = State.input
    val submits = Submits(Vector(
      Submit(OkSubmitKind),
      Submit(CancelSubmitKind)
    ))
    val hidden = Hidden(
      Some(scenario.marshall)
    )
    PropertyFormModel(uri, method, schema, data, hidden, submits)
  }
}

case class EntityCreateAction() extends Action {
  def apply(p: StateTransitionParcel): StateTransitionParcel = {
    val scenario = p.scenario
    val rsc = p.domainEntityType
    val data = p.inputFormParameters
    val ctx = p.executionContext
    val id = ctx.createEntity(rsc, data)
    p.withState(State.end)
  }
}

case class EntityUpdateAction() extends Action {
  def apply(p: StateTransitionParcel): StateTransitionParcel = {
    val scenario = p.scenario
    val rsc = p.domainEntityType
    val id = p.domainEntityId
    val data = p.inputFormParameters
    val ctx = p.executionContext
    ctx.updateEntity(rsc, id, data)
    p.withState(State.end)
  }
}

trait Guard {
  def isAccept(evt: StateTransitionParcel): Boolean
}

case object OkEventGuard extends Guard {
  def isAccept(p: StateTransitionParcel): Boolean = p.event.isInstanceOf[OkEvent]
}

case object CancelEventGuard extends Guard {
  def isAccept(p: StateTransitionParcel): Boolean = p.event.isInstanceOf[CancelEvent]
}

case object BackEventGuard extends Guard {
  def isAccept(p: StateTransitionParcel): Boolean = p.event.isInstanceOf[BackEvent]
}

case object InputEventGuard extends Guard {
  def isAccept(p: StateTransitionParcel): Boolean = p.event.isInstanceOf[InputEvent]
}

// case class GuardActionHandler(
//   slots: Vector[GuardActionHandler.Slot]
// ) {
//   def get(evt: Event): Option[Action] = slots.find(_.isAccept(evt)).map(_.action)
// }

// object GuardActionHandler {
//   case class Slot(guard: Guard, action: Action) {
//     def isAccept(evt: Event): Boolean = guard.isAccept(evt)
//   }
// }

case class Transition(
  slots: Vector[Transition.Slot]
) {
  def applyOption(p: StateTransitionParcel): Option[StateTransitionParcel] =
    slots.toStream.flatMap(_.applyOption(p)).headOption
}

object Transition {
  case class Slot(guard: Guard, actions: Seq[Action]) {
    def applyOption(p: StateTransitionParcel): Option[StateTransitionParcel] =
      if (guard.isAccept(p))
        Some(actions./:(p)((z, x) => x.apply(z)))
      else
        None
  }
}
