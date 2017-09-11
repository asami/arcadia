package arcadia.model

import scala.xml.NodeSeq
import java.net.URI
import org.goldenport.record.v2.{Record, Schema}
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.values.ResourceName
import org.goldenport.util.StringUtils
import arcadia._
import arcadia.view._
import arcadia.view.ViewEngine._

/*
 * @since   Jul. 29, 2017
 * @version Aug. 30, 2017
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

case class IndexModel(
  pageName: Option[I18NString],
  headline: Option[I18NElement],
  resources: List[(String, ResourceListModel)],
  carousel: Option[CarouselModel]
) extends Model with IPageModel {
  def toRecord: Record = ??? // NotImplementedYetDefect().RAISE
  def render(strategy: RenderStrategy): NodeSeq = ??? // NotImplementedYetDefect().RAISE
  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty

  def getResourceList(name: String): Option[ResourceListModel] = resources.find(_._1 == name).map(_._2)
}

case class CarouselModel()
object CarouselModel {

}

case class QueueDashboardModel(
  title: Option[I18NElement],
  running: RecordsCardModel,
  waiting: RecordsCardModel,
  hold: RecordsCardModel,
  done: RecordsCardModel
) extends Model with IQueueDashboardModel with ISectionModel with IComponentModel {
  val caption = None
  def toRecord = ??? // NotImplementedYetDefect().RAISE
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

trait IResourceDetailModel { self: Model =>
}

trait IResourceListModel { self: Model =>
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

trait DashboardModelBase extends Model with IDashboardModel with IPageModel with ISectionModel with IComponentModel {
}

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

case class ResourceDetailModel(
  caption: Option[I18NElement],
  schema: Option[Schema],
  resource: ResourceName,
  record: Record
) extends Model with IResourceDetailModel with IComponentModel {
   protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_OBJECT -> ViewObject.create(record),
    PROP_VIEW_RECORD -> ViewRecord.create(record)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = throw new UnsupportedOperationException()
}
object ResourceDetailModel {
  def apply(
    resource: ResourceName,
    record: Record
  ): ResourceDetailModel = ResourceDetailModel(
    Some(I18NElement(resource.v)), None, resource, record
  )
}

case class ResourceListModel(
  caption: Option[I18NElement],
  resource: ResourceName,
  schema: Option[Schema],
  records: List[Record],
  transfer: Transfer
) extends Model with IResourceListModel with IComponentModel {
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_LIST -> records.map(ViewObject.create),
    PROP_VIEW_RECORD -> records.map(ViewRecord.create)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = throw new UnsupportedOperationException()
}
object ResourceListModel {
  def apply(
    resource: ResourceName,
    records: List[Record],
    transfer: Transfer
  ): ResourceListModel = ResourceListModel(
    Some(I18NElement(resource.v)), resource, None, records, transfer
  )

  def empty(name: String): ResourceListModel = ResourceListModel(
    ResourceName(name),
    Nil,
    Transfer.empty
  )
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
  def render(strategy: RenderStrategy): NodeSeq = throw new UnsupportedOperationException()
}
object RecordModel {
  def apply(
    record: Record
  ): RecordModel = RecordModel(None, None, record)
}

case class RecordsModel(
  caption: Option[I18NElement],
  schema: Option[Schema],
  records: List[Record],
  tableKind: TableKind
) extends Model with IRecordModel with IComponentModel {
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
object RecordsModel {
  def apply(records: Seq[Record]): RecordsModel = RecordsModel(None, None, records.toList, StandardTable)
  def apply(caption: I18NElement, records: Seq[Record]): RecordsModel =
    RecordsModel(Some(caption), None, records.toList, StandardTable)
  def apply(caption: I18NElement, schema: Schema, records: Seq[Record]): RecordsModel =
    RecordsModel(Some(caption), Some(schema), records.toList, StandardTable)
}

case class RecordsCardModel(
  records: RecordsModel,
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
  ){
    protected def render_Content: NodeSeq =
      card(imageTop, header, footer, records.render(strategy.content.tiny))
  }.apply
}
object RecordsCardModel {
  def apply(title: I18NElement, schema: Schema, records: Seq[Record]): RecordsCardModel =
    RecordsCardModel(RecordsModel(None, Some(schema), records.toList, DashboardTable), None, Some(TitleDescription(title)))
}
