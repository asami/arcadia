package arcadia

import org.goldenport.record.v2.Record
import org.goldenport.values.ResourceName
import arcadia.view._
import arcadia.view.ViewEngine._
import arcadia.model._

/*
 * @since   Jul. 16, 2017
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
trait Command {
//  def viewBindings(parcel: Parcel): Map[String, AnyRef] = Map.empty
//  def toRecord: Record = Record.empty // TODO
  def getModel: Option[Model] = None
  def getUseLayout: Option[Boolean] = None
}

case class IndexCommand() extends Command {
}

case class AssetsCommand(pathname: String) extends Command {
  override val getUseLayout = Some(false)
}

case class SetupCommand() extends Command {
}

case class DashboardCommand() extends Command {
}

case class ResourceDetailCommand(
  resource: ResourceName,
  record: Record
) extends Command {
  override lazy val getModel = Some(ResourceDetailModel(resource, record))
}

case class ResourceListCommand(
  resource: ResourceName,
  records: List[Record],
  transfer: Transfer
) extends Command {
  override lazy val getModel = Some(ResourceListModel(resource, records, transfer))
}
object ResourceListCommand {
  def apply(
    resource: ResourceName,
    records: Seq[Record],
    transfer: Transfer
  ): ResourceListCommand = ResourceListCommand(resource, records.toList, transfer)
}

case class RecordCommand(
  record: Record
) extends Command {
  override lazy val getModel = Some(RecordModel(record))
}

case class RecordsCommand(
  records: List[Record]
) extends Command {
  override lazy val getModel = Some(RecordsModel(records))
}

object RecordsCommand {
  def apply(records: Seq[Record]): RecordsCommand = RecordsCommand(records.toList)
}
