package arcadia

import org.goldenport.exception.RAISE
import org.goldenport.values.ResourceName
import org.goldenport.record.v2.Record
import arcadia.context._
import arcadia.controller.ControllerEngine
import arcadia.view.ViewEngine

/*
 * @since   Jul. 15, 2017
 *  version Aug. 29, 2017
 * @version Sep.  2, 2017
 * @author  ASAMI, Tomoharu
 */
class WebEngine(
  val platform: PlatformContext,
  val application: WebApplication,
  val extend: List[WebEngine]
) {
  val rule: WebApplicationRule = extend./:(application.config.toRule)(_ complement _.application.config.toRule)
  val view: ViewEngine = new ViewEngine(platform, application.view, extend.map(_.view))
  val controller: ControllerEngine = new ControllerEngine(application.controller, extend.map(_.controller))

  def apply(p: Parcel): Content = {
    val parcel = p.withApplicationRule(rule)
    val a = controller.apply(parcel)
    view.apply(a)
  }

  def render(p: Parcel): String = {
    apply(p) match {
      case m: StringContent => m.string
      case m: BinaryContent => RAISE.noReachDefect
    }
  }

  // def apply(command: Command)(implicit context: ExecutionContext): Content = {
  //   val parcel = Parcel(command, context)
  //   apply(parcel)
  // }

  // def render(command: Command)(implicit context: ExecutionContext): String = {
  //   val parcel = Parcel(command, context)
  //   apply(parcel) match {
  //     case m: StringContent => m.string
  //     case m: BinaryContent => NoReachDefect().RAISE
  //   }
  // }

  // def grid(records: Seq[Record], transfer: TransferDoc)(implicit context: ExecutionContext): String = {
  //   val resource = context.resourceName.map(ResourceName(_)) getOrElse {
  //     NoReachDefect().RAISE
  //   }
  //   val command = ResourceListCommand(resource, records, transfer)
  //   render(command)
  // }

  // def detail(record: Record)(implicit context: ExecutionContext): String = {
  //   val resource = context.resourceName.map(ResourceName(_)) getOrElse {
  //     NoReachDefect().RAISE
  //   }
  //   val command = ResourceDetailCommand(resource, record)
  //   render(command)
  // }

  // def record(record: Record)(implicit context: ExecutionContext): String = {
  //   val command = RecordCommand(record)
  //   render(command)
  // }

  // def records(records: Seq[Record])(implicit context: ExecutionContext): String = {
  //   val command = RecordsCommand(records)
  //   render(command)
  // }

  def shutdown(): Unit = view.shutdown()
}
object WebEngine {
}
