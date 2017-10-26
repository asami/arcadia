package arcadia.model

import scala.xml.{NodeSeq, Group, Text}
import java.net.URI
import java.util.Locale
import org.goldenport.exception.RAISE
import org.goldenport.record.v2._
import org.goldenport.record.v2.util.RecordUtils
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.util.StringUtils
import com.asamioffice.goldenport.text.UString
import arcadia._
import arcadia.context._
import arcadia.view._
import arcadia.view.ViewEngine._
import arcadia.view.tag.Tag
import arcadia.scenario.Event
import arcadia.domain._

/*
 * @since   Jul. 29, 2017
 *  version Aug. 30, 2017
 *  version Sep. 27, 2017
 * @version Oct. 25, 2017
 * @author  ASAMI, Tomoharu
 */
trait Model {
  private lazy val _feature_name = StringUtils.objectToUnderscoreName("Model", this)
  def featureName: String = _feature_name
  def featureNameAliases: Set[String] = Set.empty
  def expiresKind: Option[ExpiresKind]
  def toRecord: Record // for API
  def apply(strategy: RenderStrategy): Content = XmlContent(render(strategy), expiresKind)
  def render(strategy: RenderStrategy): NodeSeq
  final def viewBindings(strategy: RenderStrategy): Map[String, AnyRef] = Map(
    PROP_VIEW_MODEL -> ViewModel(this, strategy)
  ) ++ view_Bindings(strategy) // for Template

  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef]
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
  def render(strategy: RenderStrategy) = RAISE.notImplementedYetDefect
  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty
}

case class ErrorModel(
  code: Int,
  message: Option[I18NElement],
  exception: Option[Throwable],
  topUri: Option[URI],
  backUri: Option[URI]
) extends Model {
  val expiresKind = Some(NoCacheExpires)
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = error(code, message, exception, topUri, backUri)
  }.apply
  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty
}
object ErrorModel {
  def create(parcel: Parcel, e: Throwable): ErrorModel = {
    val code = parcel.context.fold {
      ExecutionContext.toCode(e)
    } { ctx =>
      ctx.toCode(e)
    }
    val backuri = _back_uri(parcel)
    ErrorModel(code, None, Some(e), None, backuri)
  }
  def create(parcel: Parcel, m: String): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(500, Some(I18NElement(m)), None, None, backuri)
  }
  def notFound(parcel: Parcel, m: String): ErrorModel = {
    val backuri = _back_uri(parcel)
    val msg = I18NElement(m)
    ErrorModel(404, Some(msg), None, None, backuri)
  }
  def create(parcel: Parcel, evt: scenario.Event): ErrorModel = RAISE.notImplementedYetDefect

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

case class IndexModel(
  pageName: Option[I18NString],
  headline: Option[I18NElement],
  entities: List[(String, EntityListModel)],
  carousel: Option[CarouselModel],
  expiresKind: Option[ExpiresKind] = Some(AgilePageExpires)
) extends Model with IPageModel {
  def toRecord: Record = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = RAISE.notImplementedYetDefect
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
  done: TableCardModel,
  expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
) extends Model with IQueueDashboardModel with ISectionModel with IComponentModel {
  val caption = None
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
  protected def view_Bindings(strategy: RenderStrategy) = Map.empty // TODO
}

case class QueueDashboardSetModel(
  title: Option[I18NElement],
  total: QueueDashboardModel,
  queues: List[QueueDashboardModel],
  expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
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
  record: Record,
  expiresKind: Option[ExpiresKind] = Some(AgilePageExpires)
) extends Model with IEntityDetailModel with IComponentModel {
  def getEntityType = Some(entityType)
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_OBJECT -> ViewObject.create(record),
    PROP_VIEW_RECORD -> ViewRecord.create(record)
  )
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy) = throw new UnsupportedOperationException()
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
  caption: Option[I18NElement],
  entityType: DomainEntityType,
  getSchema: Option[Schema],
  records: List[Record],
  transfer: Transfer,
  tableKind: Option[TableKind] = None,
  expiresKind: Option[ExpiresKind] = Some(AgilePageExpires)
) extends Model with IEntityListModel with IComponentModel {
  override def getEntityType: Option[DomainEntityType] = Some(entityType)
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_LIST -> records.map(ViewObject.create),
    PROP_VIEW_RECORD -> records.map(ViewRecord.create)
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

  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table(Renderer.TableOrder(tableKind, getSchema, getEntityType, records))
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
  caption: Option[I18NElement],
  getSchema: Option[Schema],
  record: Record,
  expiresKind: Option[ExpiresKind] = None
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
  caption: Option[I18NElement],
  getSchema: Option[Schema],
  records: List[Record],
  tableKind: Option[TableKind] = None,
  expiresKind: Option[ExpiresKind] = None
) extends ITableModel with IComponentModel {
  def getEntityType = None
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create)
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
  caption: Option[I18NElement],
  getSchema: Option[Schema],
  records: List[Record],
  tableKind: Option[TableKind],
  expiresKind: Option[ExpiresKind] = None
) extends ITableModel with IComponentModel {
  def getEntityType = None
  protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create)
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

case class RecordModel(
  caption: Option[I18NElement],
  getSchema: Option[Schema],
  record: Record,
  expiresKind: Option[ExpiresKind] = None
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
  records: List[Record],
  expiresKind: Option[ExpiresKind] = None
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
  imageTop: Option[Picture] = None,
  header: Option[TitleLine] = None,
  footer: Option[TitleLine] = None,
  expiresKind: Option[ExpiresKind] = None
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
      card(imageTop, header, footer, records.render(strategy.withScopeContent.withSizeTiny))
  }.apply
}
object TableCardModel {
  def apply(title: I18NElement, schema: Schema, records: Seq[Record]): TableCardModel =
    TableCardModel(TableModel(None, Some(schema), records.toList, Some(DashboardTable)), None, Some(TitleLine(title)))
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
  val caption = None
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq =
      property_input_form(uri, method, schema, record, hidden, submit)
  }.apply
  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty
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
  val caption = None
  def toRecord: Record = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq =
      property_confirm_form(uri, method, schema, record, hidden, submit)
  }.apply
  protected def view_Bindings(strategy: RenderStrategy): Map[String, AnyRef] = Map.empty
}

/*
 *
 */
case class TitleLine(
  title: Option[I18NElement],
  subTitle: Option[I18NElement]
) {
  def toOption: Option[TitleLine] =
    if (title.isEmpty)
      None
    else
      Some(this)
}
object TitleLine {
  def apply(title: I18NElement): TitleLine = TitleLine(Some(title), None)
}

case class Picture( // HTML5 picture
  // TODO source
  // TODO map/area
  src: URI,
  alt: Option[I18NString],
  href: Option[URI],
  size: Option[Int],
  height: Option[Int],
  width: Option[Int]
)
object Picture {
  def apply(src: URI): Picture = Picture(src, None, None, None, None, None)
}

case class Card(
  imagetop: Option[Picture],
  header: Option[TitleLine],
  footer: Option[TitleLine],
  content: NodeSeq
)
object Card {
  def apply(pic: Picture, header: TitleLine, content: NodeSeq): Card = Card(Some(pic), Some(header), None, content)

  def apply(pic: Picture, header: Option[TitleLine], content: NodeSeq): Card = Card(Some(pic), header, None, content)
}

case class Submits(submits: Vector[Submit])
object Submits {
  def apply(p: Submit, ps: Submit*): Submits = Submits(
    (p +: ps).toVector
  )
}

case class Submit(kind: SubmitKind, label: I18NString) {
  def name = ScenarioCommand.PROP_SUBMIT_PREFIX + kind.name
  def value(locale: Locale) = label(locale)
}
object Submit {
  def apply(kind: SubmitKind): Submit = Submit(kind, kind.label)
}

sealed trait SubmitKind {
  def name: String
  def label: I18NString = I18NString(UString.capitalize(name))
}
case object OkSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_OK
}
case object CancelSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_CANCEL
}
case object InputSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_INPUT
}
case object CreateSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_CREATE
}
case object UpdateSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_UPDATE
}
case object DELETESubmitKind extends SubmitKind {
  def name: String = Event.EVENT_DELETE
}
case object BackSubmitKind extends SubmitKind {
  def name: String = Event.EVENT_BACK
}

case class Hidden(
//  event: Option[String],
  scenario: Option[String]
) {
  def render: NodeSeq = <div>{
    scenario.map(x => <input type="hidden" name={ScenarioCommand.PROP_SCENARIO} value={x}></input>).toList
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
