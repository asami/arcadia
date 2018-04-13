package arcadia.context

import scala.xml._
import java.util.Locale
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.goldenport.json.JsonUtils.Implicits._
import org.goldenport.i18n.I18NString
import org.goldenport.record.v2._
import org.goldenport.util.StringUtils
import Column.Form
import org.goldenport.xml.XmlUtils

/*
 * @since   Jan. 22, 2018
 *  version Feb. 18, 2018
 * @version Apr. 10, 2018
 * @author  ASAMI, Tomoharu
 */
case class Parameter(
  name: String,
  datatype: Option[DataType] = None,
  multiplicity: Option[Multiplicity] = None,
  label: Option[I18NString] = None,
  placeholder: Option[I18NString] = None,
  value: Option[String] = None,
  readonly: Option[Boolean] = None,
  hidden: Option[Boolean] = None
) {
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

  def takeLabel(locale: Locale) = StringUtils.label(label.map(_.as(locale)), name)

  def toColumn: Column = Column(
    name,
    datatype getOrElse XToken,
    multiplicity getOrElse MOne,
    i18nLabel = label,
    form = Column.Form(
      placeholder,
      value,
      hidden getOrElse false,
      readonly getOrElse false
    )
  )

  // protected def take_datatype: String =
  //   if (hidden.getOrElse(false))
  //     "hidden"
  //   else
  //     "text" // TODO

  // protected def take_value: String = value getOrElse ""
}

object Parameter {
  object json {
    import Schema.json._
    implicit val ParameterFormat = Json.format[Parameter]
  }
}
