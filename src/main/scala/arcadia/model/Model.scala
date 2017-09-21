package arcadia.model

import scala.xml.{NodeSeq, Group}
import java.net.URI
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.{Record, Schema}
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.util.StringUtils
import arcadia._
import arcadia.view._
import arcadia.view.ViewEngine._
import arcadia.domain._

/*
 * @since   Jul. 29, 2017
 *  version Aug. 30, 2017
 * @version Sep. 21, 2017
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

trait IEntityDetailModel { self: Model =>
}

trait IEntityListModel { self: Model =>
}

trait IRecordModel { self: Model =>
}

trait IRecordsModel { self: Model =>
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

case class IndexModel(
  pageName: Option[I18NString],
  headline: Option[I18NElement],
  entitys: List[(String, EntityListModel)],
  carousel: Option[CarouselModel]
) extends Model with IPageModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = RAISE.notImplementedYetDefect
  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty

  def getEntityList(name: String): Option[EntityListModel] = entitys.find(_._1 == name).map(_._2)
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
  schema: Option[Schema],
  klass: DomainEntityType,
  record: Record
) extends Model with IEntityDetailModel with IComponentModel {
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
    Some(I18NElement(klass.v)), None, klass, record
  )
}

case class EntityListModel(
  caption: Option[I18NElement],
  klass: DomainEntityType,
  schema: Option[Schema],
  records: List[Record],
  transfer: Transfer
) extends Model with IEntityListModel with IComponentModel {
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_LIST -> records.map(ViewObject.create),
    PROP_VIEW_RECORD -> records.map(ViewRecord.create)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = throw new UnsupportedOperationException()
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
  schema: Option[Schema],
  record: Record
) extends Model with IRecordModel with IComponentModel {
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORD -> ViewRecord.create(record)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = property_sheet(schema, record)
  }.apply
}

case class PropertyTableModel(
  caption: Option[I18NElement],
  schema: Option[Schema],
  records: List[Record]
) extends Model with IRecordModel with IComponentModel {
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = property_table(schema, records)
  }.apply
}

case class TableModel(
  caption: Option[I18NElement],
  schema: Option[Schema],
  records: List[Record],
  tableKind: TableKind
) extends Model with IRecordsModel with IComponentModel {
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table(tableKind, schema, records)
  }.apply
}
object TableModel {
  def apply(records: Seq[Record]): TableModel = TableModel(None, None, records.toList, StandardTable)
  def apply(caption: I18NElement, records: Seq[Record]): TableModel =
    TableModel(Some(caption), None, records.toList, StandardTable)
  def apply(caption: I18NElement, schema: Schema, records: Seq[Record]): TableModel =
    TableModel(Some(caption), Some(schema), records.toList, StandardTable)
}

case class RecordModel(
  caption: Option[I18NElement],
  schema: Option[Schema],
  record: Record
) extends Model with IRecordModel with IComponentModel {
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORD -> ViewRecord.create(record)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table_record(schema, record)
  }.apply
}
object RecordModel {
  def apply(
    record: Record
  ): RecordModel = RecordModel(None, None, record)
}

case class RecordsModel(
  caption: Option[I18NElement],
  schema: Option[Schema],
  records: List[Record]
) extends Model with IRecordsModel with IComponentModel {
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table_records(schema, records)
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
