package arcadia

import org.goldenport.record.v2.{Record, Schema}
import org.goldenport.i18n.I18NElement
import arcadia.domain._
import arcadia.view._
import arcadia.view.ViewEngine._
import arcadia.model._
import arcadia.scenario.Scenario

/*
 * @since   Jul. 16, 2017
 *  version Aug. 29, 2017
 * @version Sep. 23, 2017
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

case class MaterialCommand(pathname: String) extends Command {
}

case class SetupCommand() extends Command {
}

case class DashboardCommand() extends Command {
}

case class EntityDetailCommand(
  klass: DomainEntityType,
  record: Record
) extends Command {
  override lazy val getModel = Some(EntityDetailModel(klass, record))
}

case class EntityListCommand(
  klass: DomainEntityType,
  records: List[Record],
  transfer: Transfer
) extends Command {
  override lazy val getModel = Some(EntityListModel(klass, records, transfer))
}
object EntityListCommand {
  def apply(
    klass: DomainEntityType,
    records: Seq[Record],
    transfer: Transfer
  ): EntityListCommand = EntityListCommand(klass, records.toList, transfer)
}

case class PropertySheetCommand(
  caption: Option[I18NElement],
  schema: Option[Schema],
  record: Record
) extends Command {
  override lazy val getModel = Some(PropertySheetModel(caption, schema, record))
}

case class PropertyTableCommand(
  caption: Option[I18NElement],
  schema: Option[Schema],
  records: List[Record]
) extends Command {
  override lazy val getModel = Some(PropertyTableModel(caption, schema, records))
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

case class ScenarioCommand(
  scenario: Scenario
) {
}
