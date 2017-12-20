package arcadia.model

import scala.xml.{NodeSeq, Group, Text}
import java.net.URI
import java.util.Locale
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.record.v2.util.RecordUtils
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.trace.TraceContext
import org.goldenport.util.StringUtils
import arcadia._
import arcadia.context._
import arcadia.view._
import arcadia.view.ViewEngine._
import arcadia.view.tag.{Tag, Expression}
import arcadia.scenario.Event
import arcadia.domain._

/*
 * @since   Jul. 29, 2017
 *  version Aug. 30, 2017
 *  version Sep. 27, 2017
 *  version Oct. 31, 2017
 *  version Nov. 13, 2017
 * @version Dec. 21, 2017
 * @author  ASAMI, Tomoharu
 */
trait Model {
  private lazy val _feature_name = StringUtils.objectToUnderscoreName("Model", this)
  def featureName: String = _feature_name
  def featureNameAliases: Set[String] = Set.empty
  def expiresKind: Option[ExpiresKind]
  def toRecord: Record // for API
  def show: String = s"${getClass.getSimpleName}"

  def apply(strategy: RenderStrategy): Content = XmlContent(render(strategy), expiresKind)
  def render(strategy: RenderStrategy): NodeSeq
  final def viewBindings(strategy: RenderStrategy): Map[String, AnyRef] = Map(
    PROP_VIEW_MODEL -> ViewModel(this, strategy)
  ) ++ view_Bindings(strategy) // for Template

  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty
  def isActiveFeature(p: String): Boolean = p == featureName || featureNameAliases.contains(p)

  //
  protected final def as_caption(s: Option[I18NElement], p: Option[String]): Option[I18NElement] =
    p.map(I18NElement(_)).orElse(s)

  protected final def as_schema(s: Option[Schema], p: Option[List[String]]): Option[Schema] =
    (s, p) match {
      case (None, None) => None
      case (Some(s), None) => Some(s)
      case (None, Some(s)) => Some(make_schema(s))
      case (Some(l), Some(r)) => Some(adjust_schema(l, r))
    }

  protected def make_schema(p: List[String]): Schema =
    Schema(p.map(x => Column(x, XString, MZeroOne)))

  protected def adjust_schema(l: Schema, r: List[String]): Schema = {
    case class Z(xs: Vector[Column] = Vector.empty) {
      def r = l.copy(columns = xs)
      def +(rhs: String) = l.getColumn(rhs).fold(this)(x => Z(xs :+ x))
    }
    r./:(Z())(_+_).r
  }

  protected final def get_table_kind(s: Option[TableKind], p: Option[String]): Option[TableKind] =
    p.fold(s)(TableKind.get)

  protected final def as_table_kind(s: TableKind, p: Option[String]): TableKind =
    p.fold(s)(x => TableKind.get(x) getOrElse s)
}

object Model {
  val candidates: Vector[ModelClass] = Vector(EntityDetailModel, EntityListModel, PropertySheetModel, PropertyTableModel, AutoModel)
  val candidatesStream = candidates.toStream

  def get(param: ModelParameter, response: Response): Option[Model] = candidatesStream.flatMap(_.get(param, response)).headOption
}

trait ModelClass {
  def name: String = StringUtils.classNameToHypenName("Model", this)
  def get(param: ModelParameter, response: Response): Option[Model]
}

case class ModelParameter(
  kind: Option[String]
) {
  def isKind(p: String) = kind.fold(false)(_ == p)
}

trait IPageModel { self: Model =>
  def pageName: Option[I18NString]
  def headline: Option[I18NElement]
}

trait ISectionModel { self: Model =>
  def title: Option[I18NElement]
}

trait IComponentModel { self: Model =>
  def caption: Option[I18NElement] = None
}

trait IAtomicModel { self: Model =>
}

trait IOrganismModel extends IAtomicModel { self: Model =>
}

trait IMoleculeModel extends IAtomicModel { self: Model =>
}

trait IAtomModel extends IAtomicModel { self: Model =>
}

trait ISheetModel extends IRecordModel {
}

trait ITableModel extends IRecordsModel {
  def tableKind: Option[TableKind]
  def getEntityType: Option[DomainEntityType]
  def records: List[Record]
  def thead: TableHeadModel
  def tbody: TableBodyModel
  def withSchemaKind(
    schema: Option[List[String]],
    kind: Option[String]
  ): ITableModel
  def withSchemaKind(
    schema: Option[List[String]],
    kind: TableKind
  ): ITableModel
  def withCaptionSchemaKind(
    caption: Option[String],
    schema: Option[List[String]],
    kind: Option[String]
  ): ITableModel
}

trait IEntityDetailModel extends ISheetModel { self: Model =>
}

trait IEntityListModel extends ITableModel { self: Model =>
}

trait IRecordModel extends Model {
  def getEntityType: Option[DomainEntityType]
  def getSchema: Option[Schema]
  def record: Record
}

trait IRecordsModel extends Model {
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
  val expiresKind = None
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = empty_block
  }.apply

}

case class ErrorModel(
  code: Int,
  message: Option[I18NElement],
  exception: Option[Throwable],
  topUri: Option[URI],
  backUri: Option[URI],
  trace: Option[TraceContext]
) extends Model {
  val expiresKind = Some(NoCacheExpires)
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = error(code, message, exception, topUri, backUri, trace)
  }.apply
}
object ErrorModel {
  def create(parcel: Parcel, code: Int): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(code, None, None, None, backuri, parcel.trace)
  }
  def create(parcel: Parcel, code: Int, e: Throwable): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(code, None, Some(e), None, backuri, parcel.trace)
  }
  def create(parcel: Parcel, e: Throwable): ErrorModel = {
    val code = parcel.context.fold {
      ExecutionContext.toCode(e)
    } { ctx =>
      ctx.toCode(e)
    }
    val backuri = _back_uri(parcel)
    ErrorModel(code, None, Some(e), None, backuri, parcel.trace)
  }
  def create(parcel: Parcel, m: String): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(500, Some(I18NElement(m)), None, None, backuri, parcel.trace)
  }
  def create(parcel: Parcel, evt: scenario.Event): ErrorModel = RAISE.notImplementedYetDefect
  def create(code: Int, message: Option[String], exception: Option[Throwable]): ErrorModel =
    ErrorModel(code, message.map(I18NElement(_)), exception, None, None, None)
  def notFound(parcel: Parcel, m: String): ErrorModel = {
    val backuri = _back_uri(parcel)
    val msg = I18NElement(m)
    ErrorModel(404, Some(msg), None, None, backuri, parcel.trace)
  }
  def unauthorized(parcel: Parcel): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(401, None, None, None, backuri, parcel.trace)
  }

  private def _back_uri(parcel: Parcel): Option[URI] = None // TODO
}

sealed trait ValueModel extends Model with IAtomicModel {
  def datatype: DataType
}
case class SingleValueModel(datatype: DataType, v: Option[Any]) extends ValueModel {
  val expiresKind = None
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = Text(v.toString) // TODO
}
case class MultipleValueModel(datatype: DataType, v: List[Any]) extends Model with ValueModel {
  val expiresKind = None
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = Text(v.toString) // TODO
}
object ValueModel {
  def create(column: Column, rec: Record): ValueModel =
    if (column.isSingle)
      SingleValueModel(column.datatype, rec.getOne(column.name))
    else
      MultipleValueModel(column.datatype, rec.effectiveList(column.name))

  def create(p: String): ValueModel = SingleValueModel(XString, Some(p))
}

object AutoModel extends ModelClass {
  def get(param: ModelParameter, response: Response): Option[Model] = {
    if (param.kind.isEmpty)
      response.getRecords.map(PropertyTableModel(_)) orElse {
        response.getRecord.map(PropertySheetModel(_))
      }
    else
      None
  }
}

case class WidgetModel(
  name: String,
  expression: Expression,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IComponentModel {
  override val featureName = s"widget__$name"
  def toRecord: Record = RAISE.notImplementedYetDefect
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_WIDGET -> ViewWidget(WidgetModel.this, strategy)
  )
  def render(strategy: RenderStrategy) = new Renderer(strategy) {
    protected def render_Content: NodeSeq = widget(WidgetModel.this)
  }.apply
}

case class IndexModel(
  pageName: Option[I18NString],
  headline: Option[I18NElement],
  entities: List[(String, EntityListModel)],
  carousel: Option[CarouselModel],
  expiresKind: Option[ExpiresKind] = Some(AgilePageExpires)
) extends Model with IPageModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = RAISE.notImplementedYetDefect

  def getEntityList(name: String): Option[EntityListModel] = entities.find(_._1 == name).map(_._2)
}

case class CarouselModel(
  pictures: List[Picture],
  expiresKind: Option[ExpiresKind] = None
) extends Model with IComponentModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = carousel(pictures)
  }.apply
}
object CarouselModel {
}

case class BannerModel(
  pictures: List[Picture],
  expiresKind: Option[ExpiresKind] = None
) extends Model with IComponentModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = banner(pictures)
  }.apply
}

case class BadgeModel(
  badge: Badge,
  expiresKind: Option[ExpiresKind] = Some(PrivatePageExpires)
) extends Model with IComponentModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = this.badge(BadgeModel.this.badge)
  }.apply
}

case class NoticeModel(
  getXml: Option[Xml],
  expiresKind: Option[ExpiresKind] = None // TODO
) extends Model with IComponentModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = getXml.map(_.apply(locale)).getOrElse(Group(Nil)) // TODO background
  }.apply
}

case class XmlModel(
  xml: Xml,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IComponentModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = xml(locale)
  }.apply
}

case class QueueDashboardModel(
  title: Option[I18NElement],
  running: TableCardModel,
  waiting: TableCardModel,
  hold: TableCardModel,
  done: TableCardModel,
  expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
) extends Model with IQueueDashboardModel with ISectionModel with IComponentModel {
  def toRecord = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, title, caption
  ) {
    protected def render_Content: NodeSeq = <div>
    <div>{running.render(strategy.withScopeSection)}</div>
    <div>{waiting.render(strategy.withScopeSection)}</div>
    <div>{hold.render(strategy.withScopeSection)}</div>
    <div>{done.render(strategy.withScopeSection)}</div>
    </div>
  }.apply
}

case class QueueDashboardSetModel(
  title: Option[I18NElement],
  total: QueueDashboardModel,
  queues: List[QueueDashboardModel],
  expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
) extends Model with IQueueDashboardSetModel with ISectionModel with IComponentModel {
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
}

trait DashboardModelBase extends Model with IDashboardModel with IPageModel with ISectionModel with IComponentModel {
}

case class EntityDetailModel(
  override val caption: Option[I18NElement],
  entityType: DomainEntityType,
  getSchema: Option[Schema],
  record: Record,
  expiresKind: Option[ExpiresKind] = Some(AgilePageExpires)
) extends Model with IEntityDetailModel with IComponentModel {
  def getEntityType = Some(entityType)
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_OBJECT -> ViewObject.create(record, strategy),
    PROP_VIEW_RECORD -> ViewRecord.create(record, strategy)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = entity_property_sheet(entityType, getSchema, record)
  }.apply
}
object EntityDetailModel extends ModelClass {
  def apply(
    klass: DomainEntityType,
    record: Record
  ): EntityDetailModel = EntityDetailModel(
    Some(I18NElement(klass.v)), klass, None, record
  )

  def get(param: ModelParameter, response: Response): Option[Model] =
    response.entityType.flatMap { entitytype =>
      if (param.isKind(name))
        Some(EntityDetailModel(entitytype, response.getRecord.getOrElse(Record.empty)))
      else
        None
    }
}

case class EntityListModel(
  override val caption: Option[I18NElement],
  entityType: DomainEntityType,
  getSchema: Option[Schema],
  records: List[Record],
  transfer: Transfer,
  tableKind: Option[TableKind] = None,
  expiresKind: Option[ExpiresKind] = Some(AgilePageExpires),
  dataHref: Option[URI] = None
) extends Model with IEntityListModel with IComponentModel {
  override def getEntityType: Option[DomainEntityType] = Some(entityType)
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_LIST -> records.map(ViewObject.create(_, strategy)),
    PROP_VIEW_RECORD -> records.map(ViewRecord.create(_, strategy))
  )
  def toRecord: Record = throw new UnsupportedOperationException()

  def get(i: Int): Option[EntityDetailModel] = records.lift(i).map(EntityDetailModel(entityType, _))

  def withSchemaKind(
    schema: Option[List[String]],
    kind: Option[String]
  ): EntityListModel = copy(
    getSchema = as_schema(getSchema, schema),
    tableKind = get_table_kind(tableKind, kind)
  )

  def withSchemaKind(
    schema: Option[List[String]],
    kind: TableKind
  ): EntityListModel = copy(
    getSchema = as_schema(getSchema, schema),
    tableKind = Some(kind)
  )

  def withCaptionSchemaKind(
    caption: Option[String],
    schema: Option[List[String]],
    kind: Option[String]
  ): EntityListModel = copy(
    caption = as_caption(this.caption, caption),
    getSchema = as_schema(getSchema, schema),
    tableKind = get_table_kind(tableKind, kind)
  )

  def withDataHref(p: Option[URI]): EntityListModel = copy(dataHref = p)

  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table(Renderer.TableOrder(tableKind, getSchema, getEntityType, dataHref, records))
  }.apply
  lazy val effectiveSchema = getSchema.getOrElse(RecordUtils.buildSchema(records))
  lazy val thead: TableHeadModel = TableHeadModel(effectiveSchema, tableKind)
  lazy val tbody: TableBodyModel = TableBodyModel(Some(effectiveSchema), records, tableKind)
}
object EntityListModel extends ModelClass {
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

  def get(param: ModelParameter, response: Response): Option[Model] =
    response.entityType.flatMap { entitytype =>
      def transfer = response.transfer getOrElse {
        RAISE.notImplementedYetDefect
      }
      if (param.isKind(name))
        Some(apply(entitytype, response.getRecords.getOrElse(Nil), transfer))
      else
        None
    }
}

case class PropertySheetModel(
  override val caption: Option[I18NElement],
  getSchema: Option[Schema],
  record: Record,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IRecordModel with IComponentModel {
  def getEntityType = None
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORD -> ViewRecord.create(record, strategy)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = property_sheet(getSchema, record)
  }.apply
}
object PropertySheetModel extends ModelClass {
  def apply(caption: String, schema: Schema, record: Record): PropertySheetModel = PropertySheetModel(
    Some(I18NElement(caption)), Some(schema), record
  )
  def apply(record: Record): PropertySheetModel = PropertySheetModel(
    None, None, record
  )

  def get(param: ModelParameter, response: Response): Option[Model] =
    response.entityType.flatMap { entitytype =>
      if (param.isKind(name))
        Some(EntityDetailModel(entitytype, response.getRecord.getOrElse(Record.empty)))
      else
        None
    }
}

case class PropertyTableModel(
  override val caption: Option[I18NElement],
  getSchema: Option[Schema],
  records: List[Record],
  tableKind: Option[TableKind] = None,
  expiresKind: Option[ExpiresKind] = None
) extends ITableModel with IComponentModel {
  def getEntityType = None
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create(_, strategy))
  )

  def withSchemaKind(
    schema: Option[List[String]],
    kind: Option[String]
  ): PropertyTableModel = copy(
    getSchema = as_schema(getSchema, schema),
    tableKind = get_table_kind(tableKind, kind)
  )

  def withSchemaKind(
    schema: Option[List[String]],
    kind: TableKind
  ): PropertyTableModel = copy(
    getSchema = as_schema(getSchema, schema),
    tableKind = Some(kind)
  )

  def withCaptionSchemaKind(
    caption: Option[String],
    schema: Option[List[String]],
    kind: Option[String]
  ): PropertyTableModel = copy(
    caption = as_caption(this.caption, caption),
    getSchema = as_schema(getSchema, schema),
    tableKind = get_table_kind(tableKind, kind)
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
}
object PropertyTableModel extends ModelClass {
  def apply(records: List[Record]): PropertyTableModel = PropertyTableModel(
    None, None, records
  )

  def get(param: ModelParameter, response: Response): Option[Model] =
    if (param.isKind(name))
      Some(PropertyTableModel(response.getRecords.getOrElse(Nil)))
    else
      None
}

case class TableModel(
  override val caption: Option[I18NElement],
  getSchema: Option[Schema],
  records: List[Record],
  tableKind: Option[TableKind],
  expiresKind: Option[ExpiresKind] = None
) extends ITableModel with IComponentModel {
  def getEntityType = None
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create(_, strategy))
  )
  def withSchemaKind(
    schema: Option[List[String]],
    kind: Option[String]
  ): TableModel = copy(
    getSchema = as_schema(getSchema, schema),
    tableKind = get_table_kind(tableKind, kind)
  )

  def withSchemaKind(
    schema: Option[List[String]],
    kind: TableKind
  ): TableModel = copy(
    getSchema = as_schema(getSchema, schema),
    tableKind = Some(kind)
  )

  def withCaptionSchemaKind(
    caption: Option[String],
    schema: Option[List[String]],
    kind: Option[String]
  ): TableModel = copy(
    caption = as_caption(this.caption, caption),
    getSchema = as_schema(getSchema, schema),
    tableKind = get_table_kind(tableKind, kind)
  )

  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table(strategy.tableKind(tableKind), getSchema, records)
  }.apply
  lazy val effectiveSchema = getSchema.getOrElse(RecordUtils.buildSchema(records))
  lazy val thead: TableHeadModel = TableHeadModel(effectiveSchema, tableKind)
  lazy val tbody: TableBodyModel = TableBodyModel(Some(effectiveSchema), records, tableKind)
}
object TableModel {
  def apply(records: Seq[Record]): TableModel = TableModel(None, None, records.toList, None)
  def apply(caption: I18NElement, records: Seq[Record]): TableModel =
    TableModel(Some(caption), None, records.toList, None)
  def apply(caption: I18NElement, schema: Schema, records: Seq[Record]): TableModel =
    TableModel(Some(caption), Some(schema), records.toList, None)
}

case class TableHeadModel(
  schema: Schema,
  tableKind: Option[TableKind],
  expiresKind: Option[ExpiresKind] = None
) extends Model with IOrganismModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = table_head(strategy.tableKind(tableKind), schema)
  }.apply
}

case class TableHeadRecordModel(
  schema: Schema,
  tableKind: TableKind,
  expiresKind: Option[ExpiresKind] = None
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
  tableKind: Option[TableKind],
  expiresKind: Option[ExpiresKind] = None
) extends Model with IRecordsModel with IOrganismModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = {
      val s = getSchema.getOrElse(RecordUtils.buildSchema(records))
      table_body(strategy.tableKind(tableKind), s, records)
    }
  }.apply
  def getEntityType = None
  def record = toRecord
}

case class TableBodyRecordsModel(
  getSchema: Option[Schema],
  records: List[Record],
  tableKind: TableKind,
  expiresKind: Option[ExpiresKind] = None
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
  tableKind: TableKind,
  expiresKind: Option[ExpiresKind] = None
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
  column: Column,
  value: ValueModel,
  tableKind: TableKind,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IMoleculeModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = table_body_record_data(column, tableKind, value)
  }.apply
  def getEntityType = None
  def record = toRecord
}

case class TableBodyRecordDataModel(
  column: Column,
  value: ValueModel,
  tableKind: TableKind,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IMoleculeModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = table_body_record_data(column, tableKind, value)
  }.apply
  def getEntityType = None
  def record = toRecord
}

case class CardModel(
  card: Card,
  override val featureName: String
) extends Model with IRecordModel with IComponentModel {
  def getEntityType = None
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_CARD -> ViewCard.create(card, strategy),
    PROP_VIEW_OBJECT -> ViewObject.create(record, strategy),
    PROP_VIEW_RECORD -> ViewRecord.create(record, strategy)
  )
  def getSchema: Option[Schema] = None
  def record = card.record getOrElse Record.empty
  def expiresKind = None
  def render(strategy: RenderStrategy) = RAISE.noReachDefect
  def toRecord = RAISE.notImplementedYetDefect
}

case class RecordModel(
  override val caption: Option[I18NElement],
  getSchema: Option[Schema],
  record: Record,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IRecordModel with IComponentModel {
  def getEntityType = None
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORD -> ViewRecord.create(record, strategy)
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
  override val caption: Option[I18NElement],
  getSchema: Option[Schema],
  records: List[Record],
  expiresKind: Option[ExpiresKind] = None
) extends Model with IRecordsModel with IComponentModel {
  def getEntityType = None
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create(_, strategy))
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
  imageTop: Option[Picture] = None,
  header: Option[TitleLine] = None,
  footer: Option[TitleLine] = None,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IComponentModel {
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.records.map(ViewRecord.create(_, strategy))
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq =
      card(imageTop, header, footer, records.render(strategy.withScopeContent.withSizeTiny))
  }.apply
}
object TableCardModel {
  def apply(title: I18NElement, schema: Schema, records: Seq[Record]): TableCardModel =
    TableCardModel(TableModel(None, Some(schema), records.toList, Some(DashboardTable)), None, Some(TitleLine.create(title)))
}

case class SearchBoxModel(
  schema: Schema
) extends Model with IFormModel with IComponentModel {
  val expiresKind: Option[ExpiresKind] = None
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(strategy) {
    protected def render_Content: NodeSeq = searchbox_form(
      Renderer.SearchBox(schema)
    )
  }.apply
}

case class PropertyInputFormModel(
  uri: URI,
  method: Method,
  schema: Schema,
  record: Record,
  hidden: Hidden,
  submit: Submits,
  expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
) extends Model with IFormModel with IComponentModel {
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq =
      property_input_form(uri, method, schema, record, hidden, submit)
  }.apply
}

case class PropertyConfirmFormModel(
  uri: URI,
  method: Method,
  schema: Schema,
  record: Record,
  hidden: Hidden,
  submit: Submits,
  expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
) extends Model with IFormModel with IComponentModel {
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq =
      property_confirm_form(uri, method, schema, record, hidden, submit)
  }.apply
}

case class OperationOutcomeModel(
  request: Request,
  response: Response
) extends Model {
  val expiresKind = Some(NoCacheExpires)
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = operation_outcome(request, response)
  }.apply
}
