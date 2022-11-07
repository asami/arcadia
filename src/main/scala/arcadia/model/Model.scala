package arcadia.model

import scala.xml.{NodeSeq, Group, Text}
import java.net.URI
import java.util.Locale
import org.goldenport.context.{Conclusion => CConclusion, StatusCode}
import org.goldenport.exception.RAISE
import org.goldenport.collection.{NonEmptyVector, VectorMap}
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v2.{Record => _, Conclusion => RConclusion, _}
import org.goldenport.record.v2.util.RecordUtils
import org.goldenport.record.util.AnyUtils
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.value._
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
 *  version Dec. 21, 2017
 *  version Jan. 22, 2018
 *  version Feb. 17, 2018
 *  version Mar. 21, 2018
 *  version Apr.  8, 2018
 *  version Jul. 23, 2018
 *  version Aug. 31, 2018
 *  version Sep.  1, 2018
 *  version Nov.  7, 2018
 *  version Apr. 30, 2019
 *  version May.  1, 2019
 *  version Aug.  5, 2019
 *  version Mar. 21, 2020
 *  version Apr. 18, 2020
 *  version May. 28, 2020
 *  version Jun.  1, 2020
 *  version May. 28, 2022
 *  version Sep. 27, 2022
 *  version Oct. 30, 2022
 * @version Nov.  6, 2022
 * @author  ASAMI, Tomoharu
 */
trait Model {
  private lazy val _feature_name = StringUtils.objectToUnderscoreName("Model", this)
  def featureName: String = _feature_name
  def featureNameAliases: Set[String] = Set.empty
  def expiresKind: Option[ExpiresKind]
  def toRecord: IRecord // for API
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
  val candidates: Vector[ModelClass] = Vector(EntityDetailModel, EntityListModel, PropertySheetModel, PropertyTableModel, AutoModel, ErrorModel)
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
  def records: List[IRecord]
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
  def record: IRecord
  def getDomainObjectId: Option[DomainObjectId] = DomainObjectId.get(record, getEntityType)
  def getDomainEntityId: Option[DomainEntityId] = getDomainObjectId.collect {
    case m: DomainEntityId => m
  }
}

trait IRecordsModel extends Model {
  def getEntityType: Option[DomainEntityType]
  def getSchema: Option[Schema]
  def records: List[IRecord]
}

trait IDashboardModel { self: Model =>
}

trait IQueueDashboardModel extends IDashboardModel { self: Model =>
}

trait IQueueDashboardSetModel extends IDashboardModel { self: Model =>
}

trait IQueueSetModel { self: Model =>
}

trait FormModel { self: Model =>
  def action: URI
  def method: Method
  def get(name: String): Option[Any]
  def getPlaceholder(name: String): Option[String]
  def conclusion: FormModel.Conclusion
  def setError(p: RConclusion): Model
}
object FormModel {
  sealed trait InputType extends NamedValueInstance {
  }
  object InputType extends EnumerationClass[InputType] {
    val elements = Vector(
      ButtonInput,
      CheckboxInput,
      ColorInput,
      DateInput,
      DatetimeLocalInput,
      EmailInput,
      FileInput,
      HiddenInput,
      ImageInput,
      MonthInput,
      NumberInput,
      PasswordInput,
      RadioInput,
      RangeInput,
      ResetInput,
      SearchInput,
      SubmitInput,
      TelInput,
      TextInput,
      TimeInput,
      UrlInput,
      WeekInput
    )
  }
  case object ButtonInput extends InputType {
    val name = "button"
  }
  case object CheckboxInput extends InputType {
    val name = "checkbox"
  }
  case object ColorInput extends InputType {
    val name = "color"
  }
  case object DateInput extends InputType {
    val name = "date"
  }
  case object DatetimeLocalInput extends InputType {
    val name = "datetime-local"
  }
  case object EmailInput extends InputType {
    val name = "email"
  }
  case object FileInput extends InputType {
    val name = "file"
  }
  case object HiddenInput extends InputType {
    val name = "hidden"
  }
  case object ImageInput extends InputType {
    val name = "image"
  }
  case object MonthInput extends InputType {
    val name = "month"
  }
  case object NumberInput extends InputType {
    val name = "number"
  }
  case object PasswordInput extends InputType {
    val name = "password"
  }
  case object RadioInput extends InputType {
    val name = "radio"
  }
  case object RangeInput extends InputType {
    val name = "range"
  }
  case object ResetInput extends InputType {
    val name = "reset"
  }
  case object SearchInput extends InputType {
    val name = "search"
  }
  case object SubmitInput extends InputType {
    val name = "submit"
  }
  case object TelInput extends InputType {
    val name = "tel"
  }
  case object TextInput extends InputType {
    val name = "text"
  }
  case object TimeInput extends InputType {
    val name = "time"
  }
  case object UrlInput extends InputType {
    val name = "url"
  }
  case object WeekInput extends InputType {
    val name = "week"
  }

  case class Field(
    name: String,
    label: Option[String] = None,
    inputType: Option[InputType] = None,
    id: Option[String] = None,
    value: Option[String] = None,
    placeholder: Option[String] = None,
    datatype: DataType = XString,
    constraints: List[Constraint] = Nil
  ) {
    def withValue(p: String) = copy(value = Some(p))
    def withPlaceholder(p: String) = copy(placeholder = Some(p))

    def toColumn: Column = Column(
      name,
      datatype,
      label = label,
      form = Column.Form.create(placeholder, value)
    )
  }
  object Field {
    def hidden(name: String, value: String): Field = Field(name, inputType = Some(HiddenInput), value = Some(value))
    def password(name: String, label: Option[String]): Field = Field(name, label, Some(PasswordInput))
    def submit(label: Option[String]): Field = Field("submit", label, Some(SubmitInput))
  }

  case class Conclusion(
    warnings: Option[NonEmptyVector[I18NString]] = None,
    errors: Option[NonEmptyVector[I18NString]] = None,
    warningsForProperty: VectorMap[String, I18NString] = VectorMap.empty,
    errorsForProperty: VectorMap[String, I18NString] = VectorMap.empty
  ) {
    lazy val allWarnings: Vector[I18NString] = warnings.map(_.vector).getOrElse(Vector.empty) ++ warningsForProperty.values
    lazy val allErrors: Vector[I18NString] = errors.map(_.vector).getOrElse(Vector.empty) ++ errorsForProperty.values

    def getAllWarnings: Option[NonEmptyVector[I18NString]] = NonEmptyVector.createOption(allWarnings)
    def getAllErrors: Option[NonEmptyVector[I18NString]] = NonEmptyVector.createOption(allErrors)
    def getWarnings: Option[NonEmptyVector[I18NString]] = warnings
    def getErrors: Option[NonEmptyVector[I18NString]] = errors
    def isError: Boolean = errors.nonEmpty || errorsForProperty.nonEmpty
    def isGeneralError: Boolean = errors.nonEmpty
    def getGeneralErrorMessage(locale: Locale): Option[String] = getErrors.map(_.map(_.as(locale)).mkString(";"))
    def isGeneralWarning: Boolean = warnings.nonEmpty
    def getGeneralWarningMessage(locale: Locale): Option[String] = getWarnings.map(_.map(_.as(locale)).mkString(";"))
    def isError(name: String): Boolean = errorsForProperty.contains(name)
    def getErrorMessage(locale: Locale, name: String): Option[String] = errorsForProperty.get(name).map(_.as(locale))
    def isWarning(name: String): Boolean = warningsForProperty.contains(name)
    def getWarningMessage(locale: Locale, name: String): Option[String] = warningsForProperty.get(name).map(_.as(locale))
  }
  object Conclusion {
    val empty = Conclusion()

    def apply(p: Invalid): Conclusion = Conclusion(errors = Some(NonEmptyVector(p.i18nMessage)))

    def apply(p: RConclusion): Conclusion = {
      Conclusion(
        p.warnings,
        p.errors,
        VectorMap(
          p.warningForProperty.map {
            case (k, v) => (k.name, v.i18nMessage)
          }
        ),
        VectorMap(
          p.errorForProperty.map {
            case (k, v) => (k.name, v.i18nMessage)
          }
        )
      )
    }
  }
}

case object EmptyModel extends Model {
  val expiresKind = None
  def toRecord: IRecord = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = empty_block
  }.apply

}

case class ErrorModel(
  conclusion: CConclusion,
  element: Option[I18NElement],
  invalid: Option[Invalid],
  topUri: Option[URI],
  backUri: Option[URI],
  trace: Option[TraceContext]
) extends Model {
  def code = conclusion.code
  def message: Option[I18NElement] = element orElse Some(I18NElement(conclusion.messageI18N))
  def exception = conclusion.exception
  val expiresKind = Some(NoCacheExpires)
  def toRecord: IRecord = RAISE.notImplementedYetDefect
  override def apply(strategy: RenderStrategy): Content = XmlContent(render(strategy), expiresKind, code)
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = error(code, message, exception, invalid, topUri, backUri, trace)
  }.apply
}
object ErrorModel extends ModelClass {
  def apply(
    code: Int,
    element: Option[I18NElement],
    exception: Option[Throwable],
    invalid: Option[Invalid],
    topUri: Option[URI],
    backUri: Option[URI],
    trace: Option[TraceContext]
  ): ErrorModel = ErrorModel(
    CConclusion(StatusCode(code), exception = exception),
    element,
    invalid,
    topUri,
    backUri,
    trace
  ) // compatibility

  def create(parcel: Parcel, code: Int): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(code, None, None, None, None, backuri, parcel.trace)
  }
  def create(parcel: Parcel, code: Int, e: Throwable): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(code, None, Some(e), None, None, backuri, parcel.trace)
  }
  def create(parcel: Parcel, e: Throwable): ErrorModel = {
    val code = parcel.context.fold {
      ExecutionContext.toCode(e)
    } { ctx =>
      ctx.toCode(e)
    }
    val backuri = _back_uri(parcel)
    ErrorModel(code, None, Some(e), None, None, backuri, parcel.trace)
  }
  def create(parcel: Parcel, m: Invalid): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(400, None, None, Some(m), None, backuri, parcel.trace)
  }
  def create(parcel: Parcel, m: String): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(500, Some(I18NElement(m)), None, None, None, backuri, parcel.trace)
  }
  def create(parcel: Parcel, evt: scenario.Event): ErrorModel = RAISE.notImplementedYetDefect
  def create(parcel: Parcel, c: CConclusion): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(c, None, None, None, backuri, parcel.trace)
  }
  def create(code: Int, message: Option[String], exception: Option[Throwable]): ErrorModel =
    ErrorModel(code, message.map(I18NElement(_)), exception, None, None, None, None)
  def create(res: Response): ErrorModel =
    ErrorModel(res.code, res.getString.map(I18NElement(_)), None, None, None, None, None)
  def notFound(parcel: Parcel, m: String): ErrorModel = {
    val backuri = _back_uri(parcel)
    val msg = I18NElement(m)
    ErrorModel(404, Some(msg), None, None, None, backuri, parcel.trace)
  }
  def unauthorized(parcel: Parcel): ErrorModel = {
    val backuri = _back_uri(parcel)
    ErrorModel(401, None, None, None, None, backuri, parcel.trace)
  }

  private def _back_uri(parcel: Parcel): Option[URI] = None // TODO

  def get(param: ModelParameter, response: Response): Option[Model] =
    if (response.code > 300)
      Some(create(response.code, None, None))
    else
      None
}

sealed trait ValueModel extends Model with IAtomicModel {
  def datatype: DataType
}
case class SingleValueModel(datatype: DataType, v: Option[Any]) extends ValueModel {
  val expiresKind = None
  def toRecord: IRecord = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = Text(v.fold("")(AnyUtils.toString))
}
case class MultipleValueModel(datatype: DataType, v: List[Any]) extends Model with ValueModel {
  val expiresKind = None
  def toRecord: IRecord = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = Text(v.toString) // TODO
}
object ValueModel {
  def create(column: Column, rec: IRecord): ValueModel =
    if (column.isSingle)
      SingleValueModel(column.datatype, rec.get(column.name))
    else
      MultipleValueModel(column.datatype, rec.takeList(column.name))

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
  def toRecord: IRecord = RAISE.notImplementedYetDefect
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
  def toRecord: IRecord = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy) = RAISE.notImplementedYetDefect

  def getEntityList(name: String): Option[EntityListModel] = entities.find(_._1 == name).map(_._2)
}

case class CarouselModel(
  pictures: List[Picture],
  expiresKind: Option[ExpiresKind] = None
) extends Model with IComponentModel {
  def toRecord: IRecord = RAISE.notImplementedYetDefect
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
  def toRecord: IRecord = RAISE.notImplementedYetDefect
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
  def toRecord: IRecord = RAISE.notImplementedYetDefect
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
  def toRecord: IRecord = RAISE.notImplementedYetDefect
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
  def toRecord: IRecord = RAISE.notImplementedYetDefect
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
  record: IRecord,
  expiresKind: Option[ExpiresKind] = Some(AgilePageExpires)
) extends Model with IEntityDetailModel with IComponentModel {
  def getEntityType = Some(entityType)
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_OBJECT -> ViewObject.create(record, strategy),
    PROP_VIEW_RECORD -> ViewRecord.create(record, strategy)
  )
  def toRecord: IRecord = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = entity_property_sheet(entityType, getSchema, record)
  }.apply
}
object EntityDetailModel extends ModelClass {
  def apply(
    klass: DomainEntityType,
    record: IRecord
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
  records: List[IRecord],
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
  def toRecord: IRecord = throw new UnsupportedOperationException()

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
  lazy val effectiveSchema = getSchema.getOrElse(IRecord.makeSchema(records))
  lazy val thead: TableHeadModel = TableHeadModel(effectiveSchema, tableKind)
  lazy val tbody: TableBodyModel = TableBodyModel(Some(effectiveSchema), records, tableKind)
}
object EntityListModel extends ModelClass {
  def apply(
    klass: DomainEntityType,
    records: List[IRecord],
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
  record: IRecord,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IRecordModel with IComponentModel {
  def getEntityType = None
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORD -> ViewRecord.create(record, strategy)
  )
  def toRecord: IRecord = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = property_sheet(getSchema, record)
  }.apply
}
object PropertySheetModel extends ModelClass {
  val empty = PropertySheetModel(Record.empty)

  def apply(caption: String, schema: Schema, record: IRecord): PropertySheetModel = PropertySheetModel(
    Some(I18NElement(caption)), Some(schema), record
  )
  def apply(record: IRecord): PropertySheetModel = PropertySheetModel(
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
  records: List[IRecord],
  tableKind: Option[TableKind] = None,
  expiresKind: Option[ExpiresKind] = None,
  dataHref: Option[URI] = None
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

  def withDataHref(p: Option[URI]): PropertyTableModel = copy(dataHref = p)

  def toRecord: IRecord = throw new UnsupportedOperationException()

  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = property_table(getSchema, records, dataHref)
  }.apply
  lazy val effectiveSchema = getSchema.getOrElse(IRecord.makeSchema(records))
  lazy val thead: TableHeadModel = TableHeadModel(effectiveSchema, tableKind)
  lazy val tbody: TableBodyModel = TableBodyModel(Some(effectiveSchema), records, tableKind)
}
object PropertyTableModel extends ModelClass {
  def apply(records: List[IRecord]): PropertyTableModel = PropertyTableModel(
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
  records: List[IRecord],
  tableKind: Option[TableKind],
  expiresKind: Option[ExpiresKind] = None,
  dataHref: Option[URI] = None
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

  def withDataHref(p: Option[URI]): TableModel = copy(dataHref = p)

  def toRecord: IRecord = throw new UnsupportedOperationException()

  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table(strategy.tableKind(tableKind), getSchema, records, dataHref)
  }.apply

  lazy val effectiveSchema = getSchema.getOrElse(IRecord.makeSchema(records))
  lazy val thead: TableHeadModel = TableHeadModel(effectiveSchema, tableKind)
  lazy val tbody: TableBodyModel = TableBodyModel(Some(effectiveSchema), records, tableKind)
}
object TableModel {
  def apply(records: Seq[IRecord]): TableModel = TableModel(None, None, records.toList, None)
  def apply(caption: I18NElement, records: Seq[IRecord]): TableModel =
    TableModel(Some(caption), None, records.toList, None)
  def apply(caption: I18NElement, schema: Schema, records: Seq[IRecord]): TableModel =
    TableModel(Some(caption), Some(schema), records.toList, None)
  def apply(schema: Schema, records: Seq[IRecord], kind: TableKind, expires: ExpiresKind, datahref: String): TableModel =
    TableModel(None, Some(schema), records.toList, Some(kind), Some(expires), Some(new URI(datahref)))
}

case class TableHeadModel(
  schema: Schema,
  tableKind: Option[TableKind],
  expiresKind: Option[ExpiresKind] = None
) extends Model with IOrganismModel {
  def toRecord: IRecord = RAISE.notImplementedYetDefect
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
  def toRecord: IRecord = RAISE.notImplementedYetDefect
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
  records: List[IRecord],
  tableKind: Option[TableKind],
  expiresKind: Option[ExpiresKind] = None
) extends Model with IRecordsModel with IOrganismModel {
  def toRecord: IRecord = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = {
      val s = getSchema.getOrElse(IRecord.makeSchema(records))
      table_body(strategy.tableKind(tableKind), s, records)
    }
  }.apply
  def getEntityType = None
  def record = toRecord
}

case class TableBodyRecordsModel(
  getSchema: Option[Schema],
  records: List[IRecord],
  tableKind: TableKind,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IRecordModel with IOrganismModel {
  def toRecord: IRecord = RAISE.notImplementedYetDefect
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = table(tableKind, getSchema, records, None)
  }.apply
  def getEntityType = None
  def record = toRecord
}

case class TableBodyRecordModel(
  getSchema: Option[Schema],
  record: IRecord,
  tableKind: TableKind,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IRecordModel with IOrganismModel {
  def toRecord: IRecord = record
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ){
    protected def render_Content: NodeSeq = {
      val s = getSchema.getOrElse(IRecord.makeSchema(record))
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
  def toRecord: IRecord = RAISE.notImplementedYetDefect
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
  def toRecord: IRecord = RAISE.notImplementedYetDefect
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
  record: IRecord,
  expiresKind: Option[ExpiresKind] = None
) extends Model with IRecordModel with IComponentModel {
  def getEntityType = None
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORD -> ViewRecord.create(record, strategy)
  )
  def toRecord: IRecord = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table_record(getSchema, record)
  }.apply
}
object RecordModel {
  def apply(
    record: IRecord
  ): RecordModel = RecordModel(None, None, record)
}

case class RecordsModel(
  override val caption: Option[I18NElement],
  getSchema: Option[Schema],
  records: List[IRecord],
  expiresKind: Option[ExpiresKind] = None
) extends Model with IRecordsModel with IComponentModel {
  def getEntityType = None
  override protected def view_Bindings(strategy: RenderStrategy) = Map(
    PROP_VIEW_RECORDS -> records.map(ViewRecord.create(_, strategy))
  )
  def toRecord: IRecord = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, caption
  ){
    protected def render_Content: NodeSeq = table_records(getSchema, records)
  }.apply
}
object RecordsModel {
  def apply(records: Seq[IRecord]): RecordsModel = RecordsModel(None, None, records.toList)
  def apply(caption: I18NElement, records: Seq[IRecord]): RecordsModel =
    RecordsModel(Some(caption), None, records.toList)
  def apply(caption: I18NElement, schema: Schema, records: Seq[IRecord]): RecordsModel =
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
  def toRecord: IRecord = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq =
      card(imageTop, header, footer, records.render(strategy.withScopeContent.withSizeTiny))
  }.apply
}
object TableCardModel {
  def apply(title: I18NElement, schema: Schema, records: Seq[IRecord], expires: ExpiresKind, datahref: String): TableCardModel =
    TableCardModel(
      TableModel(schema, records, DashboardTable, expires, datahref),
      None,
      Some(TitleLine.create(title))
    )
}

case class SearchBoxModel(
  searchbox: Renderer.SearchBox,
  conclusion: FormModel.Conclusion = FormModel.Conclusion.empty
) extends Model with FormModel with IComponentModel {
  val expiresKind: Option[ExpiresKind] = None
  def toRecord: IRecord = RAISE.notImplementedYetDefect
  def action = searchbox.input.action
  def method = searchbox.input.method
  def get(name: String): Option[Any] = None
  def getPlaceholder(name: String): Option[String] = None

  def setError(p: RConclusion) = copy(conclusion = FormModel.Conclusion(p))

  def render(strategy: RenderStrategy): NodeSeq = new Renderer(strategy) {
    protected def render_Content: NodeSeq = searchbox_form(searchbox)
  }.apply
}
object SearchBoxModel {
  def apply(action: URI, schema: Schema): SearchBoxModel =
    SearchBoxModel(Renderer.SearchBox(action, schema))
}

case class PropertyInputFormModel(
  action: URI,
  method: Method,
  schema: Schema,
  data: IRecord,
  hiddens: Hiddens,
  submit: Submits,
  expiresKind: Option[ExpiresKind] = Some(NoCacheExpires),
  conclusion: FormModel.Conclusion = FormModel.Conclusion.empty
) extends Model with FormModel with IComponentModel {
  lazy val toRecord: IRecord = hiddens.hiddens.complement(data)

  def get(name: String): Option[Any] = toRecord.get(name)

  def getPlaceholder(name: String): Option[String] = schema.getColumn(name).flatMap(_.form.placeholder).map(_.c)

  def setError(p: RConclusion): PropertyInputFormModel = copy(conclusion = FormModel.Conclusion(p))

  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq =
      property_input_form(action, method, schema, data, hiddens, submit)
  }.apply
}

case class PropertyConfirmFormModel(
  action: URI,
  method: Method,
  schema: Schema,
  data: IRecord,
  hiddens: Hiddens,
  submit: Submits,
  expiresKind: Option[ExpiresKind] = Some(NoCacheExpires),
  conclusion: FormModel.Conclusion = FormModel.Conclusion.empty
) extends Model with FormModel with IComponentModel {
  def toRecord: IRecord = data // TODO hiddens
  def get(name: String): Option[Any] = data.get(name)

  def getPlaceholder(name: String): Option[String] = schema.getColumn(name).flatMap(_.form.placeholder).map(_.c)

  def setError(p: RConclusion) = copy(conclusion = FormModel.Conclusion(p))

  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq =
      property_confirm_form(action, method, schema, data, hiddens, submit)
  }.apply
}

case class UpdateEntityDirectiveFormModel(
  action: URI,
  label: I18NString,
  id: DomainEntityId,
  properties: IRecord,
  isActive: Boolean,
  conclusion: FormModel.Conclusion = FormModel.Conclusion.empty,
  placeholders: IRecord = Record.empty
) extends Model with FormModel with IComponentModel {
  val expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
  def toRecord: IRecord = properties
  def method = Put
  def get(name: String): Option[Any] = properties.get(name)
  def getPlaceholder(name: String): Option[String] = placeholders.getString(name)

  def setError(p: RConclusion) = copy(conclusion = FormModel.Conclusion(p))

  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = update_entity_directive_form(action, properties)
  }.apply
}

case class InvokeDirectiveFormModel(
  action: URI,
  method: Method,
  title: Option[I18NElement],
  description: Option[I18NElement],
  submitLabel: Option[I18NElement],
  parameters: Parameters,
  arguments: IRecord,
  isActive: Boolean,
  // error: Option[Invalid] = None
  conclusion: FormModel.Conclusion = FormModel.Conclusion.empty,
  placeholders: IRecord = Record.empty
) extends Model with FormModel with IComponentModel {
  val expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
  def toRecord: IRecord = arguments
  def get(name: String): Option[Any] = arguments.get(name)
  def getPlaceholder(name: String): Option[String] = placeholders.getString(name)

  def setError(p: RConclusion) = copy(conclusion = FormModel.Conclusion(p))

  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = RAISE.notImplementedYetDefect
  }.apply
}

case class InvokeWithIdDirectiveFormModel(
  action: URI,
  method: Method,
  label: I18NString,
  id: DomainObjectId,
  properties: IRecord,
  isActive: Boolean,
  idPropertyName: Option[String],
  conclusion: FormModel.Conclusion = FormModel.Conclusion.empty,
  placeholders: IRecord = Record.empty
) extends Model with FormModel with IComponentModel {
  val expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
  def toRecord: IRecord = properties
  def get(name: String): Option[Any] = properties.get(name)
  def getPlaceholder(name: String): Option[String] = placeholders.getString(name)

  def setError(p: RConclusion) = copy(conclusion = FormModel.Conclusion(p))

  def render(strategy: RenderStrategy): NodeSeq = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = invoke_with_id_directive_form(action, properties)
  }.apply
}

case object UndefinedFormModel extends Model with FormModel {
  val expiresKind: Option[ExpiresKind] = Some(NoCacheExpires)
  val conclusion: FormModel.Conclusion = FormModel.Conclusion.empty // TODO
  def toRecord: IRecord = throw new UnsupportedOperationException()
  def action: URI = new URI("Undefined")
  def method: Method = Get
  def get(name: String): Option[Any] = None
  def getPlaceholder(name: String): Option[String] = None

  def setError(p: RConclusion) = this

  def render(strategy: RenderStrategy): NodeSeq = throw new UnsupportedOperationException()
}

case class OperationOutcomeModel(
  request: Request,
  response: Response
) extends Model with ISectionModel {
  def title = Some(I18NElement(request.operationName))
  val expiresKind = Some(NoCacheExpires)
  def toRecord: IRecord = throw new UnsupportedOperationException()
  def render(strategy: RenderStrategy) = new Renderer(
    strategy, None, None, None, None
  ) {
    protected def render_Content: NodeSeq = operation_outcome(request, response)
  }.apply
}

case class CandidatesModel(
  candidates: PowertypeClassCandidates
) extends Model {
  val expiresKind = Some(NoCacheExpires)
  def toRecord: IRecord = RAISE.noReachDefect
  def render(strategy: RenderStrategy) = RAISE.noReachDefect
}
