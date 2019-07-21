package arcadia.context

import scala.xml._
import java.util.Locale
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.goldenport.exception.RAISE
import org.goldenport.json.JsonUtils.Implicits._
import org.goldenport.i18n.I18NString
import org.goldenport.record.v3.IRecord
import org.goldenport.record.v2._
import org.goldenport.json.JsonUtils
import org.goldenport.util.StringUtils
import Column.Form
import org.goldenport.xml.XmlUtils
import arcadia.Parcel
import arcadia.model._
import arcadia.view.RenderStrategy

/*
 * @since   Jan. 22, 2018
 *  version Feb. 18, 2018
 *  version Apr. 10, 2018
 *  version Jul. 17, 2018
 *  version Apr. 28, 2019
 *  version May.  1, 2019
 * @version Jul. 20, 2019
 * @author  ASAMI, Tomoharu
 */
case class Parameter(
  name: String,
  datatype: Option[DataType] = None,
  multiplicity: Option[Multiplicity] = None,
  label: Option[I18NString] = None,
  placeholder: Option[I18NString] = None,
  value: Option[String] = None, // default value
  candidates: Option[Candidates] = None,
  constraints: Option[Constraints] = None,
  readonly: Option[Boolean] = None,
  hidden: Option[Boolean] = None
) {
  import Parameter._
  // def toInput(locale: Locale, id: String, cssclass: String): NodeSeq =
  //   XmlUtils.elementWithAttributesFixOption(
  //     "input",
  //     List(
  //       "type" -> take_datatype,
  //       "name" -> name,
  //       "value" -> take_value,
  //       "class" -> cssclass
  //     ),
  //     List(
  //       "placeholder" -> placeholder.map(_.as(locale))
  //     )
  //   )

  def setDataType(p: DataType): Parameter = copy(datatype = Some(p), candidates = None)

  def takeLabel(locale: Locale) = StringUtils.label(label.map(_.as(locale)), name)

  def toColumn(implicit ctx: RenderStrategy): Column = Column(
    name,
    _datatype,
    multiplicity getOrElse MOne,
    constraints = constraints.map(_.constraints) getOrElse Nil,
    i18nLabel = label,
    form = Column.Form(
      placeholder,
      value,
      hidden getOrElse false,
      readonly getOrElse false
    )
  )

  private def  _datatype(implicit ctx: RenderStrategy): DataType =
    datatype orElse {
      candidates.map {
        case PowertypeClassCandidates(ptc) => XPowertype(ptc)
        case PowertypeCandidates(pts) => XPowertype(PowertypeClassInstance(pts))
        case NamedCandidates(name) => RAISE.noReachDefect
      }
    } getOrElse XToken

  // protected def take_datatype: String =
  //   if (hidden.getOrElse(false))
  //     "hidden"
  //   else
  //     "text" // TODO

  // protected def take_value: String = value getOrElse ""

  def validate(p: IRecord): ValidationResult = ???
}

object Parameter {
  def create(name: String, datatype: DataType): Parameter = Parameter(name, Some(datatype))

  def create(name: String, datatype: DataType, constraints: Seq[Constraint]): Parameter =
    Parameter(name, datatype = Some(datatype), constraints = Constraints.createOption(constraints))

  def toSchema(
    ps: Seq[Parameter]
  )(implicit ctx: RenderStrategy): Schema = Schema(ps.map(_.toColumn))

  def resolve(parcel: Parcel, parameters: List[Parameter]) =
    parameters.map(_resolve(parcel, _))

  private def _resolve(parcel: Parcel, p: Parameter): Parameter =
    if (p.datatype.isDefined)
      p
    else
      p.candidates.map { x =>
        val a: DataType = x match {
          case PowertypeClassCandidates(ptc) => XPowertype(ptc)
          case PowertypeCandidates(pts) => XPowertype(PowertypeClassInstance(pts))
          case NamedCandidates(name) => XPowertype(parcel.fetchCandidates(name).candidates.powertype)
        }
        p.setDataType(a)
      }.getOrElse(p)

  object json {
    import Schema.json._
    import org.goldenport.json.JsonUtils.Implicits._
    import org.goldenport.record.v2.Constraint.json.Implicits._
    import org.goldenport.record.v2.Constraints.json.Implicits._
    import org.goldenport.record.v2.Validator.json.Implicits._

   implicit val ParameterFormat = Json.format[Parameter]
//    implicit val ParametersFormat = Json.format[Parameters]

    // implicit object ParameterFormat extends Format[Parameter] {
    //   def reads(json: JsValue): JsResult[Parameter] = json match {
    //     case m: JsObject =>
    //       val name = JsonUtils.toString("name", m)
    //       val datatype = JsonUtils.get(m, "datatype").map(Json.fromJson[DataType])
    //       val multiplicity = JsonUtils.get(m, "multiplicity").map(Json.fromJson[Multiplicity])
    //       val label = JsonUtils.get(m, "label").map(Json.fromJson[I18NString])
    //       val placeholder = JsonUtils.get(m, "placeholder").map(Json.fromJson[I18NString])
    //       val value = JsonUtils.getString("value", m)
    //       val candidates = 
    //       val constraints = ???
    //       val readonly = ???
    //       val hidden = ???
    //       JsSuccess(Parameter(name, datatype, multiplicity, label, placeholder, value, candidates, constraints, readonly, hidden))
    //     case _ => JsError(s"ParameterFormat($json)")
    //   }
    //   def writes(o: Parameter): JsValue = RAISE.notImplementedYetDefect
    // }

    implicit object ParametersFormat extends Format[Parameters] {
      def reads(json: JsValue): JsResult[Parameters] = json match {
        case JsArray(xs) =>
          case class Z(
            ps: Vector[Parameter] = Vector.empty,
            errors: Vector[JsError] = Vector.empty
          ) {
            def r = if (errors.isEmpty)
              JsSuccess(Parameters(ps.toList, Nil))
            else
              errors.head // TODO

            def +(rhs: JsValue) = Json.fromJson[Parameter](rhs) match {
              case JsSuccess(s, _) => copy(ps = ps :+ s)
              case m: JsError => copy(errors = errors :+ m)
            }
          }
          xs./:(Z())(_+_).r
        case _ => JsError(s"ParametersFormat($json)")
      }
      def writes(o: Parameters): JsValue = RAISE.notImplementedYetDefect
    }
  }
}
