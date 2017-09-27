package arcadia.model

import scala.xml.{NodeSeq, Group, Text}
import java.net.URI
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.record.v2.util.RecordUtils
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.util.StringUtils
import arcadia._
import arcadia.view._
import arcadia.view.ViewEngine._
import arcadia.domain._

/*
 * @since   Jul. 29, 2017
 *  version Aug. 30, 2017
 * @version Sep. 27, 2017
 * @author  ASAMI, Tomoharu
 */
trait Model {
  private lazy val _feature_name = StringUtils.objectToUnderscoreName("Model", this)
  def featureName: String = _feature_name
  def featureNameAliases: Set[String] = Set.empty
  def toRecord: Record // for API
  def render(strategy: RenderStrategy): NodeSeq // for HTML
  final def viewBindings(strategy: RenderStrategy): Map[String, AnyRef] = Map(
    PROP_VIEW_MODEL -> ViewModel(this, strategy)
  ) ++ view_Bindings(strategy) // for Template

  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef]
  def isActiveFeature(p: String): Boolean = p == featureName || featureNameAliases.contains(p)
}

object Model {
}

trait IPageModel { self: Model =>
  def pageName: Option[I18NString]
  def headline: Option[I18NElement]
}

trait ISectionModel { self: Model =>
  def title: Option[I18NElement]
}

trait IComponentModel { self: Model =>
  def caption: Option[I18NElement]
}

trait IAtomicModel { self: Model =>
  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty
}

trait IOrganismModel extends IAtomicModel { self: Model =>
}

trait IMoleculeModel extends IAtomicModel { self: Model =>
}

trait IAtomModel extends IAtomicModel { self: Model =>
}

trait ISheetModel extends IRecordModel { self: Model =>
}

trait ITableModel extends IRecordsModel { self: Model =>
  def tableKind: TableKind
  def thead: TableHeadModel
  def tbody: TableBodyModel
}

trait IEntityDetailModel extends ISheetModel { self: Model =>
}

trait IEntityListModel extends ITableModel { self: Model =>
}

trait IRecordModel { self: Model =>
  def getEntityType: Option[DomainEntityType]
  def getSchema: Option[Schema]
  def record: Record
}

trait IRecordsModel { self: Model =>
  def getEntityType: Option[DomainEntityType]
  def getSchema: Option[Schema]
  def records: List[Record]
}

trait IDashboardModel { self: Model =>
}

trait IQueueDashboardModel extends IDashboardModel { self: Model =>
}

trait IQueueDashboardSetModel extends IDashboardModel { self: Model =>
}

trait IQueueSetModel { self: Model =>
}

trait IFormModel { self: Model =>
}

case object EmptyModel extends Model {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = RAISE.notImplementedYetDefect
  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty
}

sealed trait ValueModel extends Model with IAtomicModel
case class SingleValueModel(datatype: DataType, v: Option[Any]) extends ValueModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = Text(v.toString) // TODO
}
case class MultipleValueModel(datatype: DataType, v: List[Any]) extends Model with ValueModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = Text(v.toString) // TODO
}
object ValueModel {
  def create(column: Column, rec: Record): ValueModel =
    if (column.isSingle)
      SingleValueModel(column.datatype, rec.getOne(column.name))
    else
      MultipleValueModel(column.datatype, rec.effectiveList(column.name))

  def create(p: String): ValueModel = SingleValueModel(XString, Some(p))
}

case class IndexModel(
  pageName: Option[I18NString],
  headline: Option[I18NElement],
  entities: List[(String, EntityListModel)],
  carousel: Option[CarouselModel]
) extends Model with IPageModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = RAISE.notImplementedYetDefect
  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty

  def getEntityList(name: String): Option[EntityListModel] = entities.find(_._1 == name).map(_._2)
}

case class CarouselModel()
object CarouselModel {

}

case class QueueDashboardModel(
  title: Option[I18NElement],
  running: TableCardModel,
  waiting: TableCardModel,
  hold: TableCardModel,
  done: TableCardModel
) extends Model with IQueueDashboardModel with ISectionModel with IComponentModel {
  val caption = None
  def toRecord = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, title, caption
  ) {
    protected def render_Content: NodeSeq = <div>
    <div>{running.render(strategy.section)}</div>
    <div>{waiting.render(strategy.section)}</div>
    <div>{hold.render(strategy.section)}</div>
    <div>{done.render(strategy.section)}</div>
    </div>
  }.apply
  protected def view_Bindings(strategy: RenderStrategy) = Map.empty // TODO
}

case class QueueDashboardSetModel(
  title: Option[I18NElement],
  total: QueueDashboardModel,
  queues: List[QueueDashboardModel]
) extends Model with IQueueDashboardSetModel with ISectionModel with IComponentModel {
  val caption = None
  def toRecord = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy) =
    <div class="container-fluid">
    <div class="row">
    <div class="col-md-12">
      <div>{total.render(strategy)}</div>
        {for (q <- queues) yield <div>q.render(strategy)</div>}
    </div>
    </div>
    </div>
  protected def view_Bindings(strategy: RenderStrategy) = Map.empty // TODO
}

trait DashboardModelBase extends Model with IDashboardModel with IPageModel with ISectionModel with IComponentModel {
}

case class EntityDetailModel(
  caption: Option[I18NElement],
  entityType: DomainEntityType,
  getSchema: Option[Schema],
  record: Record
) extends Model with IEntityDetailModel with IComponentModel {
  def getEntityType = Some(entityType)
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_OBJECT -> ViewObject.create(record),
    PROP_VIEW_RECORD -> ViewRecord.create(record)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = throw new UnsupportedOperationException()
}
object EntityDetailModel {
  def apply(
    klass: DomainEntityType,
    record: Record
  ): EntityDetailModel = EntityDetailModel(
    Some(I18NElement(klass.v)), klass, None, record
  )
}

case class EntityListModel(
  caption: Option[I18NElement],
  entityType: DomainEntityType,
  getSchema: Option[Schema],
  records: List[Record],
  transfer: Transfer
) extends Model with IEntityListModel with IComponentModel {
  def getEntityType = Some(entityType)
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_LIST -> records.map(ViewObject.create),
    PROP_VIEW_RECORD -> records.map(ViewRecord.create)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table(EntityTable, getSchema, records)
  }.apply
  lazy val effectiveSchema = getSchema.getOrElse(RecordUtils.buildSchema(records))
  lazy val thead: TableHeadModel = TableHeadModel(effectiveSchema, tableKind)
  lazy val tbody: TableBodyModel = TableBodyModel(Some(effectiveSchema), records, tableKind)
  val tableKind: TableKind = StandardTable
}
object EntityListModel {
  def apply(
    klass: DomainEntityType,
    records: List[Record],
    transfer: Transfer
  ): EntityListModel = EntityListModel(
    Some(I18NElement(klass.v)), klass, None, records, transfer
  )

  def empty(name: String): EntityListModel = EntityListModel(
    DomainEntityType(name),
    Nil,
    Transfer.empty
  )
}

case class PropertySheetModel(
  caption: Option[I18NElement],
  getSchema: Option[Schema],
  record: Record
) extends Model with IRecordModel with IComponentModel {
  def getEntityType = None
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORD -> ViewRecord.create(record)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = property_sheet(getSchema, record)
  }.apply
}
object PropertySheetModel {
  def apply(caption: String, schema: Schema, record: Record): PropertySheetModel = PropertySheetModel(
    Some(I18NElement(caption)), Some(schema), record
  )
  def apply(record: Record): PropertySheetModel = PropertySheetModel(
    None, None, record
  )
}

case class PropertyTableModel(
  caption: Option[I18NElement],
  getSchema: Option[Schema],
  records: List[Record]
) extends Model with ITableModel with IComponentModel {
  def getEntityType = None
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = property_table(getSchema, records)
  }.apply
  lazy val effectiveSchema = getSchema.getOrElse(RecordUtils.buildSchema(records))
  lazy val thead: TableHeadModel = TableHeadModel(effectiveSchema, tableKind)
  lazy val tbody: TableBodyModel = TableBodyModel(Some(effectiveSchema), records, tableKind)
  val tableKind: TableKind = StandardTable
}
object PropertyTableModel {
  def apply(records: List[Record]): PropertyTableModel = PropertyTableModel(
    None, None, records
  )
}

case class TableModel(
  caption: Option[I18NElement],
  getSchema: Option[Schema],
  records: List[Record],
  tableKind: TableKind
) extends Model with IRecordsModel with IComponentModel {
  def getEntityType = None
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table(tableKind, getSchema, records)
  }.apply
  lazy val effectiveSchema = getSchema.getOrElse(RecordUtils.buildSchema(records))
  lazy val thead: TableHeadModel = TableHeadModel(effectiveSchema, tableKind)
  lazy val tbody: TableBodyModel = TableBodyModel(Some(effectiveSchema), records, tableKind)
}
object TableModel {
  def apply(records: Seq[Record]): TableModel = TableModel(None, None, records.toList, StandardTable)
  def apply(caption: I18NElement, records: Seq[Record]): TableModel =
    TableModel(Some(caption), None, records.toList, StandardTable)
  def apply(caption: I18NElement, schema: Schema, records: Seq[Record]): TableModel =
    TableModel(Some(caption), Some(schema), records.toList, StandardTable)
}

case class TableHeadModel(
  schema: Schema,
  tableKind: TableKind
) extends Model with IOrganismModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = table_head(tableKind, schema)
  }.apply
}

case class TableHeadRecordModel(
  schema: Schema,
  tableKind: TableKind
) extends Model with IRecordModel with IOrganismModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = table_head_record(tableKind, schema)
  }.apply
  def getEntityType = None
  def record = toRecord
  def getSchema = Some(schema)
}

case class TableBodyModel(
  getSchema: Option[Schema],
  records: List[Record],
  tableKind: TableKind
) extends Model with IRecordsModel with IOrganismModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = {
      val s = getSchema.getOrElse(RecordUtils.buildSchema(records))
      table_body(tableKind, s, records)
    }
  }.apply
  def getEntityType = None
  def record = toRecord
}

case class TableBodyRecordsModel(
  getSchema: Option[Schema],
  records: List[Record],
  tableKind: TableKind
) extends Model with IRecordModel with IOrganismModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = table(tableKind, getSchema, records)
  }.apply
  def getEntityType = None
  def record = toRecord
}

case class TableBodyRecordModel(
  getSchema: Option[Schema],
  record: Record,
  tableKind: TableKind
) extends Model with IRecordModel with IOrganismModel {
  def toRecord: Record = record
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = {
      val s = getSchema.getOrElse(RecordUtils.buildSchema(record))
      table_body_record(tableKind, s, record)
    }
  }.apply
  def getEntityType = None
}

case class TableHeadRecordDataModel(
  value: ValueModel,
  tableKind: TableKind
) extends Model with IMoleculeModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = table_body_record_data(tableKind, value)
  }.apply
  def getEntityType = None
  def record = toRecord
}

case class TableBodyRecordDataModel(
  value: ValueModel,
  tableKind: TableKind
) extends Model with IMoleculeModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = table_body_record_data(tableKind, value)
  }.apply
  def getEntityType = None
  def record = toRecord
}

case class RecordModel(
  caption: Option[I18NElement],
  getSchema: Option[Schema],
  record: Record
) extends Model with IRecordModel with IComponentModel {
  def getEntityType = None
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORD -> ViewRecord.create(record)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table_record(getSchema, record)
  }.apply
}
object RecordModel {
  def apply(
    record: Record
  ): RecordModel = RecordModel(None, None, record)
}

case class RecordsModel(
  caption: Option[I18NElement],
  getSchema: Option[Schema],
  records: List[Record]
) extends Model with IRecordsModel with IComponentModel {
  def getEntityType = None
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table_records(getSchema, records)
  }.apply
}
object RecordsModel {
  def apply(records: Seq[Record]): RecordsModel = RecordsModel(None, None, records.toList)
  def apply(caption: I18NElement, records: Seq[Record]): RecordsModel =
    RecordsModel(Some(caption), None, records.toList)
  def apply(caption: I18NElement, schema: Schema, records: Seq[Record]): RecordsModel =
    RecordsModel(Some(caption), Some(schema), records.toList)
}

case class TableCardModel(
  records: TableModel,
  imageTop: Option[ImageAlt] = None,
  header: Option[TitleDescription] = None,
  footer: Option[TitleDescription] = None
) extends Model with IComponentModel {
  val caption = None
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.records.map(ViewRecord.create)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq =
      card(imageTop, header, footer, records.render(strategy.content.tiny))
  }.apply
}
object TableCardModel {
  def apply(title: I18NElement, schema: Schema, records: Seq[Record]): TableCardModel =
    TableCardModel(TableModel(None, Some(schema), records.toList, DashboardTable), None, Some(TitleDescription(title)))
}

case class PropertyFormModel(
  uri: URI,
  method: Method,
  schema: Schema,
  record: Record,
  hidden: Hidden,
  submit: Submits
) extends Model with IFormModel with IComponentModel {
  val caption = None
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq =
      property_form(uri, method, schema, record, hidden, submit)
  }.apply
  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty
}

/*
 *
 */
case class TitleDescription(
  title: Option[I18NElement],
  description: Option[I18NElement]
)
object TitleDescription {
  def apply(title: I18NElement): TitleDescription = TitleDescription(Some(title), None)
}

case class ImageAlt(
  src: URI,
  alt: Option[I18NString],
  href: Option[URI]
)
object ImageAlt {
  def apply(src: URI): ImageAlt = ImageAlt(src, None, None)
}

case class Submits(submits: Vector[Submit])

case class Submit(kind: SubmitKind) {
  def name = kind.name
}

sealed trait SubmitKind {
  def name: String
}
case object OkSubmitKind extends SubmitKind {
  def name: String = "ok"
}
case object CancelSubmitKind extends SubmitKind {
  def name: String = "cancel"
}
case object BackSubmitKind extends SubmitKind {
  def name: String = "back"
}

case class Hidden(
//  event: Option[String],
  scenario: Option[String]
) {
  def render: NodeSeq = <div>{
    scenario.map(x => <input type="hidden" name="web.scenario">{x}</input>)
  }</div>
}

sealed trait Method {
  def name: String
}
case object Get extends Method {
  def name = "GET"
}
case object Post extends Method {
  def name = "POST"
}
case object Put extends Method {
  def name = "PUT"
}
case object Delete extends Method {
  def name = "DELETE"
}
