package arcadia

import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.Schema
import org.goldenport.i18n.I18NElement
import org.goldenport.values.PathName
import arcadia.domain._
import arcadia.view._
import arcadia.view.ViewEngine._
import arcadia.model._
import arcadia.scenario.{Scenario, Event => ScenarioEvent}
import arcadia.context.{Request, PlatformExecutionContext}

/*
 * @since   Jul. 16, 2017
 *  version Aug. 29, 2017
 *  version Sep. 25, 2017
 *  version Oct. 24, 2017
 *  version Nov. 13, 2017
 *  version Dec. 21, 2017
 *  version Jan.  7, 2018
 *  version Mar. 13, 2018
 *  version Aug. 31, 2018
 *  version Apr. 30, 2019
 *  version Mar. 23, 2020
 * @version Apr. 17, 2020
 * @author  ASAMI, Tomoharu
 */
trait Command {
//  def viewBindings(parcel: Parcel): Map[String, AnyRef] = Map.empty
//  def toRecord: Record = Record.empty // TODO
  def getModel: Option[Model] = None
  def getLayout: Option[LayoutKind] = None
  def getDomainObjectId: Option[DomainObjectId] = None
  def show: String = toString()
}

case class AssetsCommand(pathname: String) extends Command {
  override val getLayout = Some(NoneLayout)
}

case class MaterialCommand(pathname: PathName) extends Command {
}
object MaterialCommand {
  def apply(p: String): MaterialCommand = MaterialCommand(PathName(p))
}

case class ViewCommand(pathname: PathName) extends Command {
}
object ViewCommand {
  def apply(p: String): ViewCommand = ViewCommand(PathName(p))
}

case class ErrorCommand(error: ErrorModel) extends Command {
  override def getModel = Some(error)
}

case class UnauthorizedCommand(
  request: Request,
  command: Option[Command]
) extends Command {
  def isGet = request.isGet
  def isMutation = request.isMutation
  override def getDomainObjectId = command.flatMap(_.getDomainObjectId) orElse request.getDomainObjectId
}
object UnauthorizedCommand {
  def apply(request: Request, command: Command): UnauthorizedCommand =
    UnauthorizedCommand(request, Some(command))
}

case class RerunCommand(parcel: Parcel) extends Command {
}

case class IndexCommand() extends Command {
}

case class SetupCommand() extends Command {
}

case class LoginCommand(
  access_token: String
) extends Command {
}

case class LogoutCommand(
) extends Command {
}

case class DashboardCommand() extends Command {
}

case class EntityDetailCommand(
  klass: DomainEntityType,
  record: IRecord
) extends Command {
  override lazy val getModel = Some(EntityDetailModel(klass, record))
}

case class EntityListCommand(
  klass: DomainEntityType,
  records: List[IRecord],
  transfer: Transfer
) extends Command {
  override lazy val getModel = Some(EntityListModel(klass, records, transfer))
}
object EntityListCommand {
  def apply(
    klass: DomainEntityType,
    records: Seq[IRecord],
    transfer: Transfer
  ): EntityListCommand = EntityListCommand(klass, records.toList, transfer)
}

case class PropertySheetCommand(
  caption: Option[I18NElement],
  schema: Schema,
  record: IRecord
) extends Command {
  override lazy val getModel = Some(PropertySheetModel(caption, Some(schema), record))
}

case class PropertyTableCommand(
  caption: Option[I18NElement],
  schema: Schema,
  records: List[IRecord]
) extends Command {
  override lazy val getModel = Some(PropertyTableModel(caption, Some(schema), records))
}

case class RecordCommand(
  record: IRecord
) extends Command {
  override lazy val getModel = Some(RecordModel(record))
}

case class RecordsCommand(
  records: List[IRecord]
) extends Command {
  override lazy val getModel = Some(RecordsModel(records))
}

object RecordsCommand {
  def apply(records: Seq[IRecord]): RecordsCommand = RecordsCommand(records.toList)
}

case class ScenarioCommand(
  scenario: Scenario,
  pathname: PathName,
  event: ScenarioEvent
) extends Command {
}
object ScenarioCommand {
  val PROP_SUBMIT_PREFIX = "$submit_"
  val PROP_SUBMIT = "$submit"
  val PROP_SCENARIO = "$scenario"
}

case class ScenarioCandidateCommand(
  path: List[String],
  query: Map[String, List[String]],
  form: Map[String, List[String]],
  exception: Option[Throwable] = None
) extends Command {
  import ScenarioCommand._
  val queryRecord = Record.createHttp(query)
  val formRecord = Record.createHttp(form)

  def name: String = path.head
  def entityName: String = path(1)
  def entityType: DomainEntityType = DomainEntityType(entityName)
  def getSubmit: Option[String] = form.keys.find(_.startsWith(PROP_SUBMIT_PREFIX)).map(_.substring(PROP_SUBMIT_PREFIX.length))
  def getScenario: Option[Scenario] = formRecord.getString(PROP_SCENARIO).map(Scenario.unmarshall)
}

case class InvokePlatformCommand(service: Any) extends Command {
}

case class InvokeOperationCommand(context: PlatformExecutionContext, request: Request) extends Command {
}
